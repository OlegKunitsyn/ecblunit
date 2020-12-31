<h1 align="center">
  <br>
    <img src="https://github.com/OlegKunitsyn/ecblunit/blob/main/icon.png?raw=true" alt="logo" width="200">
  <br>
  ECBLUnit tool
  <br>
  <br>
</h1>

<h4 align="center">Simple Unit Testing for z/OS written in IBM Enterprise COBOL.</h4>

<p align="center">
  <img src="https://github.com/OlegKunitsyn/ecblunit/workflows/ci/badge.svg?branch=main" />
</p>

### Features
* Assertions
* Continuous Integration

### Requirements
* z/OS 6.2+ account
* Zowe command-line interface

#### z/OS account
Obtain an access to a mainframe for training purposes, free of charge. [Register at IBM](https://www-40.ibm.com/events/wwe/ast/mtm/cobolvscode.nsf/enrollall?openform) and and follow the instructions. You'll be forwarded to [Open Mainframe Project Slack workspace](https://openmainframeproject.slack.com) get a password.

#### Zowe CLI
[Zowe](https://www.zowe.org) is an open-source framework that allows teams to manage, control, script, and develop on the mainframe, a part of the [Open Mainframe Project](https://www.openmainframeproject.org). Zowe provides a Command-Line Interface that lets you interact with the mainframe from your machine.
```
$ npm i -g @zowe/cli --ignore-scripts
```

Create Zowe default profile by using your mainframe credentials and setting the --reject-unauthorized flag to `false` that bypasses the certificate requirement:
```
$ zowe profiles create zosmf ztrial --host <IP> --port <PORT> --user <USER> --pass <PASSWORD> --reject-unauthorized false
Profile created successfully!
```

Verify that your profile can communicate with z/OSMF:
```
$ zowe zosmf check status
```

### Installation
Simply download [ecblunit.cbl](https://raw.githubusercontent.com/OlegKunitsyn/ecblunit/main/src/ecblunit.cbl) file or install by 
[COBOL Package Manager](https://github.com/OlegKunitsyn/cobolget):
```
$ npm install -g cobolget
$ cobolget init
$ cobolget add --debug ecblunit
$ cobolget update
$ cobolget install
```

### Usage
Upload the files to the mainframe and submit a JCL job which compiles and links them together. Executable `ECBLUNIT` awaits a list of test programs in `PARM` parameter, separated by space.
```
$ zowe zos-files upload file-to-data-set src/ecblunit.cbl <USER>.CBL
$ zowe zos-files upload file-to-data-set tests/tests.cbl <USER>.CBL
$ zowe jobs submit local-file tests/tests.jcl --view-all-spool-content
...
 ECBLUnit 1.62.6 by Olegs Kunicins and contributors.

 Time: 00:00:00
 OK
 Tests: 002, Skipped: 000
 Assertions: 047, Failures: 000, Exceptions: 000                                  
```

### Writing Tests
Tests are simple COBOL programs that allow futher execution (without `STOP RUN`). There is no code-generation tricks nor injections.
The assertions are COBOL programs and await two values - expected and actual, respectively:

```
CALL "ECBLUEQ" USING expected, actual.                                
```

More examples you may find in the `tests/tests.cbl` file.

At the moment these assertions are supported:
* ECBLUEQ for equality
* ECBLUNEQ for inequality

 Expected and actual values are limited by 32 in size. 

### Continuous Integration
ECBLUnit returns `RETURN-CODE` of the execution that is usually enough for CI pipelines. Here's an one-liner which picks the exit code from the output, on Linux:

```
zowe jobs submit local-file tests/tests.jcl --wait-for-output --rff retcode --rft string | cut -d" " -f 2                        
```

### Alternatives
ECBLUnit primarily focuses on Unit Testing - isolated COBOL programs and methods with an input and output.

Nonetheless, you may try two alternatives as well:
* `cobol-unit-test` - a paragraph-level Unit Testing framework, written by Dave Nicolette, hosted on [GitHub](https://github.com/neopragma/cobol-unit-test/wiki).
* `zUnit` - a commercial framework on the IBM mainframe platform.

### TODO
* Reporting in JUnit format
* Catch exceptions and stops
* Assert values of any length

Your contribution is always welcome!
