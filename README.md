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
  * `ECBLUEQ` checks binary equality of two 32-byte values
  * `ECBLUNEQ` checks binary inequality of two 32-byte values
  * `ECBLUREQ` checks binary equality of two any-length values represented by pointers
* Continuous Integration
* Hexadecimal diff

### Requirements
* z/OS 6.2+ account
* Zowe command-line interface

#### z/OS account
Obtain an access to a mainframe for training purposes, free of charge. [Register at IBM](https://www.ibm.com/events/wwe/ast/mtm/cobolvscode.nsf/enrollall) and follow the instructions. You'll receive a registration email with USER ID e.g. *Z82698*, IP address e.g. *192.86.32.250* and PORT e.g. *10443*. For generating the password, you need to login on [Open Mainframe Project Slack workspace](https://openmainframeproject.slack.com). After logging in add `zih` app via `Apps` menu and post `zih` a message e.g. *Hi*. The app will ask your e-mail address and userid that you have received. Post these details one by one and the app will create your PASSWORD for the next step.

#### Zowe CLI
[Zowe](https://www.zowe.org) is an open-source framework that allows teams to manage, control, script, and develop on the mainframe, a part of the [Open Mainframe Project](https://www.openmainframeproject.org). Zowe provides a Command-Line Interface that lets you interact with the mainframe from your machine.
```
$ npm i -g @zowe/cli --ignore-scripts
```

Create Zowe default `ztrial` profile by using your z/OS credentials and setting the --reject-unauthorized flag to `false` that bypasses the certificate requirement:
```
$ zowe profiles create zosmf ztrial --host <IP> --port <PORT> --user <USER ID> --pass <PASSWORD> --reject-unauthorized false
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
$ zowe zos-files upload file-to-data-set src/ecblunit.cbl <USER ID>.CBL
$ zowe zos-files upload file-to-data-set tests/tests.cbl <USER ID>.CBL
$ zowe jobs submit local-file tests/tests.jcl --view-all-spool-content
...
 ECBLUnit 1.64.8 by Olegs Kunicins and contributors.

 Time: 00:00:00
 OK
 Tests: 003, Skipped: 000
 Assertions: 050, Failures: 000, Exceptions: 000
```

### Writing Tests
Tests are simple COBOL programs that allow further execution (without `STOP RUN`). There is no code-generation tricks nor injections.
The assertions are COBOL programs and await two `PIC X(32)` values - expected and actual, respectively:
```
CALL "ECBLUEQ" USING expected actual.
CALL "ECBLUNEQ" USING expected actual.
```

The record-equality assertion awaits two `POINTER` values providing flexible comparison of any data at any position:
```
CALL "ECBLUREQ" USING
    BY CONTENT ADDRESS OF expected
    BY CONTENT ADDRESS OF actual
    BY CONTENT LENGTH OF expected.
```

More examples you may find in the `tests/tests.cbl` file.

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

Your contribution is always welcome!
