# Tests for acmt-network
`acmt-network` contains many components and interweave these components to accomplish complicated tasks.

In order to ensure `acmt-network` function as expected and ensure further edits on ACMT go smoothly, tests must be developed.

The tests describe what each component of `acmt-network` should behave. It serves as a check for `acmt-network` on different platforms as well.

## Usage
Start `acmt-network` and open the respective RStudio window.

In RStudio, set the working directory to be `workspace`.

Now there are two ways of running the tests.

### The first way 
Source `workspace/tests/tester.R`.

### The second way
Open `workspace/tests/test_acmt_network.R`.

Run all but the `setwd("../")` line.


### Notes
It is normal to have some warnings, as long as there are no errors.

The first way is useful when you want to do a comprehensive test. It will show a summary of the test results in the end.

The second way is useful when you only want to run a part of the tests. You can run the `test_that` blocks or `expect_` statements individually. 

Note that in the second way, you should be cautious running multiple `test_that` blocks together because they do not stop when there are errors, and there is no summaries. Thus it is hard to know whether any of the `test_that` fails (also ACMT prints a lot and makes the console contain a lot of information). If there is a need of running mulitple `test_that` blocks together, you can search "Failure" in the run outputs to see if any of the `test_that` fails.