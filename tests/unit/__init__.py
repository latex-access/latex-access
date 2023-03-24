import os
import sys


HERE = os.path.abspath(os.path.dirname(__file__))
MAIN_REPO_DIR = os.path.abspath(os.path.join(HERE, "..", ".."))
if MAIN_REPO_DIR not in sys.path:
    sys.path.insert(0, MAIN_REPO_DIR)
