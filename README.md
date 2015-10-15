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

<Lokathor> mac10688_, did you make a pastebin of all the terminal output between you adding the source and it not working?
<mac10688_> oh, you can't access private repo's if you don't pay?
<Lokathor> you cannot create them, but you can access them if you're invited
<mac10688_> I can do that
<Lokathor> i have access to the Handmade Hero repos, for example
<mac10688_> http://imgur.com/B4omc9T
<mac10688_> you can give me your account and I'll invite you. I'm about to go to bed though. I have a deadline looming at work and I need good sleep this week
<Lokathor> does the hello source export the Hello module in its cabal file?
<Lokathor> seems like the first thing that could have gone wrong
<bitemyapp> mac10688_: mate
<bitemyapp> mac10688_: you didn't depend on hello in call-em-up
<bitemyapp> mac10688_: I got it building.
<bitemyapp> mac10688_: I had to bump the upper bound for base (I use 7.10), add a LICENSE file to hello and call-em-up
* malaclyps (~malaclyps@unaffiliated/malaclyps) has joined #haskell-beginners
<bitemyapp> mac10688_: and I had to add a dependency on hello to the build-depends of call-em-up.
<Lokathor> it is my eternal sadness that they call it LICENSE and not LICENSE.txt by default
<bitemyapp> Lokathor: you don't need to investigate, I got it.
<bitemyapp> I'll post it as an issue to his repo too.
<bitemyapp> in case he missed this.
<mac10688_> I see it
<Lokathor> cool
<mac10688_> I'm a little confused
<bitemyapp> oh good
<mac10688_> but I'll go back and see where I may have missed  something
<bitemyapp> mac10688_: you can't import a module from a package unless you tell Cabal you want to use that package.
<bitemyapp> mac10688_: cabal sandbox add-source doesn't add the dependency to your Cabal file.
<bitemyapp> executable call-em-up
<bitemyapp>   main-is:             Main.hs
<bitemyapp>   build-depends:       base >=4.7 && <4.9
<bitemyapp>                      , hello
<bitemyapp>   hs-source-dirs:      src
<bitemyapp>   default-language:    Haskell2010
<bitemyapp> see how I added hello to that?
<mac10688_> I see that
<mac10688_> ok now that is making sense
<mac10688_> still some foginess, but I'll reflect longer tomorrow and go through the book to see if/what I missed
<bitemyapp> mac10688_: cool. let me know if there's anything we didn't cover in the book.
