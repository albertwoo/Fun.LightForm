# Fun.LightForm

This is a light form library for Fable and Xamarin Fabulous in elmish style.

nuget package:
* [Fun.LightForm.Fable](https://www.nuget.org/packages/Fun.LightForm.Fable)

# How to use please check Samples\Client\App

# Advatages
1. Easy to generate form from records (support nested records) and generate record from form
2. Build in field renderes with easy way to custormize style with classes
   ```fsharp
   field "UserName" input [
       InputProp.Label "Email"
       InputProp.ConvertTo InputValue.Text
       InputProp.LeftView (Icon.simpleIcon [ Fa.fa; Fa.``fa-mail-bulk`` ])
   ]
   ```
3. Easy to define validators
   ```fsharp
   Map.empty
   |> addValidators "Password"
       [
         required "Password is reuqired"
         maxLength 20 "This field`s max length is 20"
         (fun f -> if f.Value.ToString().Contains("password")
                   then Error [ "Passsord cannot contain password" ]
                   else Ok())
       ]
    ```
4. Added new operator `</>` to make using Fable.React dsl more elegantly
   ```fsharp
   div </> [
      Classes [
        Tw.``font-sans``
        Tw.``h-screen``
        ...
      ]
      Children [
        span </> [ 
            Classes [ Tw.``text-gray-700`` ]
            Text "test"
        ]
        ...
      ]
    ]
   ```