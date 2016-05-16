import os
import subprocess

def get_examples(path):
    files = []
    for file in os.listdir(path):
        if file.endswith(".pas"):
            files.append(file)
    return files

def run_example(path):
    print(path)
    process = subprocess.Popen('./ppc -O2 ' + path, stdout = subprocess.PIPE, shell = True)
    (out, err) = process.communicate()

if __name__ == '__main__':
    examples = get_examples('./examples/')
    for example in examples:
        path = './examples/' + example
        run_example(path)
