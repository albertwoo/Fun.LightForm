module Sample2.TestUserDomain

open Expecto
open Fun.Result
open Fun.LightForm


type UserProfileError =
    | NameIsRequired
    | NameCannotBeAdmin
    | NameIsTooLong of int
    | NameIsTooShort of int
    | TooYoung of int
    | TooOld of int
    | BobyIsNotEnough of int
    | BobyIsTooMuch of int
    | InValidBirthday


type UserName =
    private | UserName of string
    member this.Value = match this with UserName x -> x
    static member Create =
        Validation.safeCreateWithFirstError UserName [ 
            Validation.required NameIsRequired
            Validation.minLength 5 NameIsTooShort
            Validation.maxLength 10 NameIsTooLong
            Validation.not "admin" NameCannotBeAdmin
        ]
            

type UserAge =
    private | UserAge of int
    member this.Value = match this with UserAge x -> x
    static member Create =
        Validation.safeCreateWithFirstError UserAge [
            Validation.minNum 20 TooYoung
            Validation.maxNum 50 TooOld
        ]
            

type UserBirth =
    private | UserBirth of string
    member this.Value = match this with UserBirth x -> x
    static member Create = 
        Validation.safeCreateWithFirstError UserBirth [ 
            Validation.dateValidator InValidBirthday
        ]
            

type UserHobby =
    | Draw
    | Sing
    | Travel
    | Game
    | Coding
    | Crawl


type UserHobbies =
    private | UserHobbies of UserHobby list
    member this.Value = match this with UserHobbies x -> x
    static member Create = 
        Validation.safeCreateWithFirstError UserHobbies [
            Validation.seqMinLen 1 BobyIsNotEnough
            Validation.seqMaxLen 3 BobyIsTooMuch
        ]
            

type User =
    { Name: UserName
      Age: UserAge
      Hobies: UserHobbies
      Description: string
      Birthday: UserBirth }


[<Tests>]
let tests =
    testList "User domain tests demo" [
        test "required" { Expect.equal (UserName.Create "") (Error UserProfileError.NameIsRequired) "" }
        test "not" { Expect.equal (UserName.Create "admin") (Error UserProfileError.NameCannotBeAdmin) "" }
        
        test "minLength" { Expect.equal (UserName.Create "ad") (Error (UserProfileError.NameIsTooShort 5)) "" }
        test "maxLength" { Expect.equal (UserName.Create "12345678901") (Error (UserProfileError.NameIsTooLong 10)) "" }
        
        test "minNum" { Expect.equal (UserAge.Create 19) (Error (UserProfileError.TooYoung 20)) "" }
        test "maxNum" { Expect.equal (UserAge.Create 51) (Error (UserProfileError.TooOld 50)) "" }
        
        test "dateValidator" { Expect.isOk (UserBirth.Create "2020-01-01") "" }
        
        test "seqMinLen" { Expect.equal (UserHobbies.Create []) (Error (UserProfileError.BobyIsNotEnough 1)) "" }
        test "seqMaxLen" { Expect.equal (UserHobbies.Create [ Draw; Sing; Travel; Coding ]) (Error (UserProfileError.BobyIsTooMuch 3)) "" }
       
        test "test2" {
            let user =
                result {
                    let! name = UserName.Create "123456"
                    let! age = UserAge.Create 21
                    let! hobbies = UserHobbies.Create [ Draw; Sing; Travel ]
                    let! birth = UserBirth.Create "2020-01-01"
                    return
                        { Name = name
                          Age = age
                          Hobies = hobbies
                          Description = "test"
                          Birthday = birth }
                }
            Expect.isTrue (match user with Ok _ -> true | _ -> false) ""
        }
    ]
