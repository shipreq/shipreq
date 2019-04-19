Why do I have WSL vs WS?

* For maximum functional purity
* To make it easy to switch impls / 3rd-party-libs
* To make it easier to test logic
* To make it easier to simulate as a dep (eg. in tests, in BMs)

Example of balance: ProjectSpaLogic & ProjectSpaWebSocket
* Don't use WS API in WSL (too hard to test/simulate - don't want to have to mock `Session`)
* Don't create a WS abstraction
  - no benefit
  - would become quite large/complex to cover sufficient ground / be flexible
  - logic becomes more coupled to WS than otherwise
