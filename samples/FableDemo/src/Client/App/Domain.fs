[<AutoOpen>]
module rec Client.App.Domain

open System
open Fun.LightForm

type State =
  { UserProfileForm: LightForm }

type Msg =
  | UserProfileMsg of LightFormMsg


type Page =
  | Home of string
  | About
  | Blog of int option
  | Loading
  | NotFound of string


type UserProfile =
  { UserName: string
    Password: string
    Birthday: DateTime
    OptionTest1: string option
    OptionTest2: int option
    OptionTest3: DateTime option
    Roles: int list
    DefaultRole: int
    Country: int option
    Address: Address
    IsPublish: bool
    Description: string option }
  static member defaultValue =
    { UserName = "email"
      Password = "password"
      Birthday = DateTime.Now
      OptionTest1 = None
      OptionTest2 = None
      OptionTest3 = None
      Roles = []
      DefaultRole = -1
      Address = Address.defaultValue
      Country = None
      IsPublish = true
      Description = None }
and Address =
  { Country: string
    City: string
    Street: string option
    ZipCode: int }
  static member defaultValue =
    { Country = "shanghai"
      City = "shanghai"
      Street = None
      ZipCode = 12 }
