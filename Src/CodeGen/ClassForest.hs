{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Src.CodeGen.ClassForest where
import qualified Data.Map as M
import Src.Frontend.Types
import Control.Monad.State
import Control.Monad.Reader
import Parser.AbsLatte
import Src.Frontend.StaticCheck (getAttributesTypes, getMethodsTypes, isFnProp, getFnType)
import Src.CodeGen.State
import Control.Monad (forM_)
import System.IO
import Control.Monad.Except (MonadIO(liftIO))
import Data.List (intercalate, deleteFirstsBy)
import Src.CodeGen.Utils (addArgsToEnv, argsTypes, createArgString, placeLabel, isImplicitReturn)
import Src.CodeGen.GenStmts (genStmts)
import Src.CodeGen.Optimization (lcse, gcse)
import Src.CodeGen.GenExpr (createFunctionPointerType)
import qualified Src.Frontend.Types as Types


getMethodsTypesWithOgClassName :: String -> [ClassStmt] -> Methods
getMethodsTypesWithOgClassName ogname classStmts = map (\(FnProp _ fnDef@(FnDef _ t (Ident ident) args block)) -> (ogname,ident, getFnType fnDef)) (filter isFnProp classStmts)

createClassForest :: [TopDef] -> [LatteClass]
createClassForest [] = []
createClassForest topdefs = map (createClassTree topdefs ([], [])) (filter isRootClass topdefs)
    where
        isRootClass ClassDef {} = True
        isRootClass _ = False

createClassTree :: [TopDef] -> (Attributes, Methods) -> TopDef -> LatteClass
createClassTree topdefs (superClassAttrs, superClassMethods) cls = LatteClass {
    name       = ident,
    attributes = attributes,
    methods    = methods,
    subClasses = map (createClassTree topdefs (attributes, methods)) subclses,
    clsStmts   = clsStmts
}
    where
        methods = mergeMethods superClassMethods $ getMethodsTypesWithOgClassName ident clsStmts 
        attributes = superClassAttrs ++ getAttributesTypes clsStmts
        (ident, clsStmts) = getClassNameAndStmts cls
        subclses = filter (isSubClass ident) topdefs
        isSubClass name (ClassExtDef _ _ (Ident name') _) = name == name'
        isSubClass _ _ = False

mergeMethods :: Methods -> Methods -> Methods
mergeMethods superclassMethods methods = deleteFirstsBy (\(_, n, _) (_, n', _) -> n == n') superclassMethods methods ++ methods

getClassNameAndStmts (ClassExtDef _ (Ident ident) _ clsStmts) = (ident, clsStmts)
getClassNameAndStmts (ClassDef _ (Ident ident) clsStmts) = (ident, clsStmts)
getClassNameAndStmts _ = undefined

genClassesDefinitions :: String -> [LatteClass] -> GenM ()
genClassesDefinitions fileName forest = do 
    forM_ forest (genClassTree fileName)
    modify $ \st -> st { classesForest = forest }

genClassTree :: String -> LatteClass -> GenM ()
genClassTree filename cls = do
    modify $ \st -> st { classes = M.insert (name cls) cls (classes st) }
    modify $ \st -> st { currentClass = Just cls }
    let nameWithPercent = "%" ++ name cls
    let vtableType = nameWithPercent ++ "_vtable_type"
    let vtableData = "@" ++ name cls ++ "_vtable_data"
    -- create class struct
    appendWithNewline $ concat [nameWithPercent, " = type {", intercalate ", " ((vtableType ++ "*") : map (show . snd) (attributes cls)), " }"]

    -- create class vtable type
    appendWithNewline $ vtableType ++ "= type { " ++ intercalate ", " (map (createFunctionPointerType (name cls)) (methods cls)) ++ " }"
    -- create class vtable data global struct
    appendWithNewline $ concat [vtableData, " = global ", vtableType, " {\n"]
        ++ intercalate ",\n" (map (\met@(ogname, methodName, t) -> createFunctionPointerType (name cls) met ++ " @" ++ ogname ++ "_" ++ methodName) (methods cls))
        ++ "\n}\n"

    -- create constructor
    appendWithNewline $ concat ["define void @", name cls, "_constructor(", nameWithPercent, "* %this) {"]
    appendWithNewline $ concat ["\t%1 = getelementptr ", nameWithPercent, ", ", nameWithPercent, "* %this, i32 0, i32 0"]
    appendWithNewline $ concat ["\tstore ", vtableType, "* ", vtableData, ", ", vtableType, "** %1"]
    attrsInitializations <- mapM (\(ty, idx) -> initAttr nameWithPercent idx ty) (zip (map snd (attributes cls)) [1..])
    appendWithNewline $ concat attrsInitializations
    appendWithNewline "\tret void" 
    appendWithNewline "}" 

    forM_ (filter isFnProp $ clsStmts cls) (genMethod filename cls)

    forM_ (subClasses cls) (genClassTree filename) 
    where
        appendWithNewline :: String -> GenM ()
        appendWithNewline = \s -> liftIO $ appendFile filename (s ++ "\n")

isSimpleType :: LatteType -> Bool
isSimpleType Types.Int = True
isSimpleType Types.Bool= True
isSimpleType Types.Str = True
isSimpleType _ = False

initAttr :: String -> Int -> LatteType -> GenM String
initAttr nameWithPercent idx ty = do
    attrPointer <- genAddr ty
    let res = concat ["\t", show attrPointer, " = getelementptr ", nameWithPercent, ", ", nameWithPercent, "* %this, i32 0, i32 ", show idx, "\n"]
    case ty of 
        Types.Int -> return $ res ++ "\tstore i32 0, i32* " ++ show attrPointer
        Types.Str -> do
            id <- saveStringLiteral ""
            addr <- genAddr Types.Str
            return $ res
                ++ show (Just addr, IBitCast "[1 x i8]*" id "i8*")
                ++ "\n"
                ++ "\tstore i8*" ++ show addr ++ ", i8** " ++ show attrPointer
        Types.Bool -> return $ res ++ "\tstore i1 0, i1* " ++ show attrPointer
        Types.Array _ -> return $ res ++ "\tstore Arr* null, Arr** " ++ show attrPointer
        Types.StrippedCls s -> return $ res ++ "\tstore %" ++ s ++ "* null, %" ++ s ++ "** " ++ show attrPointer

genMethod :: String -> LatteClass -> ClassStmt -> GenM ()
genMethod filename cls (FnProp _ (FnDef _ t (Ident methodName) args (Block _ stmts))) = do
    let ty = stripPositionFromType t
    (addresses, env) <- addArgsToEnv args
    thisAddr <- genPointerAddr (name cls)
    (_, envWithThis) <- local (const env) $ declareVar "self" thisAddr
    let types = argsTypes args
    let argString = "%" ++ name cls ++ "* " ++ show thisAddr ++ if null types then "" else ", " ++ createArgString types addresses
    liftIO $ appendFile filename $ concat ["define ", show ty, " @", name cls, "_", methodName, "(", argString, ")", "{\n"]

    l <- addBlock []
    setBlock l
    emit $ placeLabel l
    local (const envWithThis) $ genStmts stmts
    st <- get
    when
        (isImplicitReturn st)
        (emit (Nothing, IVRet))
    blks <- gets blocks

    -- optimizations 
    -- forM_ (M.elems blks) (\b -> do
    --     b' <- lcse b
    --     setBlock $ label b'
    --     modifyBlock b'
    --     )
    -- gcse 
    --FIXME:

    blocks <- gets blocks
    forM_ (M.elems blocks) (\b -> do
        liftIO $ appendFile filename $ show b
        )

    liftIO $ appendFile filename "}\n"
    clearBlocks 


