#!/usr/bin/env python3
# Type "./runFileSaveLogs -h" for help

import os
import sys
import argparse
import getpass
import datetime
import shutil
import glob
import pathlib

# Setting working directory to this script's current directory
os.chdir(os.path.dirname(os.path.abspath(__file__)))

# Setting up argument parser
parser = argparse.ArgumentParser(description='Runs the argument R script(s) and moves the subsequent generated .Rout log files to a timestamped directory.')

# Function ensuring that the file is valid
def is_valid_file(parser, arg):
    if not os.path.exists(arg):
        parser.error("The file %s does not exist!" % arg)
    else:
        return arg

# Function ensuring that the directory is valid
def is_valid_directory(parser, arg):
    if not os.path.isdir(arg):
        parser.error("The specified path (%s) is not a directory!" % arg)
    else:
        return arg

# Additional arguments that can be added when running runFileSaveLogs
parser.add_argument("-i", "--identifier", help="Adds an identifier to the directory name where this is saved")
parser.add_argument('filenames', nargs='+', type=lambda x: is_valid_file(parser, x))

args = parser.parse_args()
args_dict = vars(args)

print(args_dict)

# Run given R Scripts
for filename in args_dict["filenames"]:
  system_call = "R CMD BATCH" + " " + filename
  os.system(system_call)

# Create the directory (and any parents) of the log files
currentUser = getpass.getuser()
currentTime = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") 
logDirPrefix = "./logs/"  
logDir = logDirPrefix + currentTime + "-" + currentUser

# If specified, adds the identifier to the directory name of the log
if args.identifier is not None:
    logDir += "-" + args.identifier

logDir += "/"  # Ensure the directory path ends with a slash

# Create the directory
pathlib.Path(logDir).mkdir(parents=True, exist_ok=True)

# Find and move all .Rout log files to this new directory
currentLogPaths = glob.glob('./*.Rout')
for currentLogPath in currentLogPaths:
    filename = os.path.basename(currentLogPath)  # Extract the filename
    shutil.move(currentLogPath, os.path.join(logDir, filename))  # Move the file

