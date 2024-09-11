## Changes

### 5.0.1
* Exposed the sampleGoldenSpecWithEncoding function to the public API
* Upgraded to rescript 11.1.4

### 5.0.0
* Support for ReScript v11 with uncurried mode

### 4.4.2
* Update bs-aeson to 4.9.0.

### 4.4.1
* Update bs-aeson to 4.8.0.

### 4.4.0
* Added goldenDirSpecWithEncoding which takes an encoding from the following set 
```
  [ `hex 
  | `utf8
  | `ascii
  | `latin1
  | `base64
  | `ucs2
  | `binary
  | `utf16le ]
```
### 4.3.0

* Update bs-aeson to 4.7.0.
* Update rescript to 10.1.2 (Remove bs-platform)

### 4.2.1

* Update bs-aeson to 4.6.0.

### 4.2.0

* Require bs-platform 8.4.2 and bs-aeson 4.4.0 as a minimum versions.

### 4.1.1

* Show details on failure

### 4.1.0

* Require bs-platform 8.0.0 and bs-aeson 4.0.0 as a minimum versions.

### 4.0.0

* Breaking change: sampleJsonRoundtripSpec behavior has change slightly. It breaks up a single encode test against a golden json file into many small encode tests and fails on the first one. Previously for large files a failure in the encoding test would crash the terminal and produce too much data to be useful. Now sampleJsonRoundtripSpec gives a simpler response and does not break the terminal.
* Update bs-platform to 5.0.6.
* Delete bs-fetch dependency.
* Delete bs-jest dependency.
* Add jest dependency ^24.8.0.
* Fork bs-jest locally to AesonSpec_Jest.ml in order to expose the assertion and modifier types.

### 3.0.0

* Breaking change: support for testing against a server has been dropped because bs-node-fetch is not compatible with the latest bs-platform. Now this package focuses on file based testing. Golden serialization files are produced by Haskell and tested by Haskell and ReasonML.
* Update bs-platform to 5.0.4.
* Update bs-fetch to 0.5.0.
* Delete bs-node-fetch dependency.
* Delete functions: `serverSpec` and `sampleGoldenAndServerSpec`.
* Delete Haskell server for server testing.
* Delete `toJsObject`, it was previously used internally but no longer needed.

### 2.2.0

* Update bs-aeson dependency to 3.1.0, bs-fetch to 3.1.0, bs-node-fetch to 3.0.0, bs-jest to 0.4.8 and bs-platform to 4.0.18.

### 2.1.0

* Update bs-aeson dependency to 3.0.0.

### 2.0.0

* Replace `Js.Result.t` with `Belt.Result.t`.
* Depends on bs-platform >= 3.1.0 and bs-aeson >= 2.0.0.

### 1.2.2

* Move bs-platform to devDependencies.

### 1.2.1

* Fix dependency issue. 1.2.0 is broken.

### 1.2.0

* Add `goldenDirSpec` function.

* Update `@glennsl/bs-jest` dependency to `0.4.1`.

* Update `bs-platform` to `2.2.0`.

### 1.1.0

* Initiate project with a set of tests functions.
