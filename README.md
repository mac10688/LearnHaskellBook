# LearnHaskellBook
Learning Haskell with HaskellBook

Context: I'm following the guide in Chapter 13.8 : Loading code from another project.

Problem: I am unable to import Hello project into call-em-up

For right now, I do not want to list the steps to how I got here. I'm hoping to identify the problem now and then I can look back to see what went wrong. If it is too much trouble to fix it as is, I will start over in the chapter and build the hello and call-em-up project over again.

Steps to reproduce my problem:

1. command line: cd into call-em-up
2. command line: cabal sandbox add-source ../hello
3. command line: cabal install --only-dependencies
4. cabl repl

Could not find module 'Hello'

Here is the screenshot of my command line history.
http://imgur.com/B4omc9T
