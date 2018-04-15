[![Build Status](https://travis-ci.org/jdee-emacs/jdee-server.png?branch=master)](https://travis-ci.org/jdee-emacs/jdee-server)

# jdee-server
JDEE Java backend

# Building:
[Maven 3](https://maven.apache.org/) is required to build jdee-server. You will need to clone jdee-server from git, run the build, copy the jdee-bundle.jar to a new directory, and point ```jdee-server-dir``` variable in Emacs to the directory containing the jar.

1. Install Maven (if you don't already have it)
2. At the terminal enter the following commands in a directory of your choice:
3. ```$ git clone https://github.com/jdee-emacs/jdee-server.git```
4. ```$ cd jdee-server```
5. ```$ mvn -Dmaven.test.skip=true package```
6. Copy ```target/jdee-bundle-${version}.jar``` to a directory of your choice (e.g. ```~/myJars```)
7. Start Emacs and enter the following commands:
8. ```M-x customize```
9. In the search field enter ```jdee-server-dir```
10. In the field next to "Jdee Server Dir:" enter the directory holding the jar from step 6 (e.g. ```~/myJars```)
11. Click the "Apply and Save" button
