import os
import subprocess
import sys
 
# A ultra simple tester, no proper error handling, no nothing

if len(sys.argv) != 1:
    print("Usage: python3 tester.py")
    # print("Tests should be in a format of two files: <test_name>.ins and <test_name>.output")
    exit(1)

# BAD

badFiles = [f for f in os.listdir("tests/bad") if f.endswith('.lat') and not f.startswith(".")]

tests_passed = 0
for inputFile in badFiles:
    p = subprocess.run('./latc ' + f'tests/bad/{inputFile}', shell=True, capture_output=True)
    if p.returncode != 0:
        tests_passed += 1
        print(f'{inputFile} OK')
    else:
        print(f'{inputFile} BAD')

print(f'\ntests passed {tests_passed}/{len(badFiles)}')
print()
# GOOD

goodFiles = [f for f in os.listdir("tests/good") if f.endswith('.lat') and not f.startswith(".")]

tests_passed = 0
for inputFile in goodFiles:
    p = subprocess.run('./latc ' + f'tests/good/{inputFile}', shell=True, capture_output=True)
    if p.returncode == 0:
        tests_passed += 1
        print(f'{inputFile} OK')
    else:
        print(f'{inputFile} BAD')

print(f'\ntests passed {tests_passed}/{len(goodFiles)}')
