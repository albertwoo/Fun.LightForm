module Sample2.Program

open Expecto

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args Sample2.TestUserDomain.tests
