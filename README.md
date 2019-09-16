# Fun.Routing

This is a routing (navigation) library for fable and inspired by Giraffe.

It supports both client side and server sider rendering (SSR).


# How to use
1. Add nuget package `Fun.Routing`
2. Generate `RouterId` and save somewhere for example in app state. I use random id to avoid concurrency issue on server side rendering.
    ```fsharp
    let defaultState =
      { RouterId = Random().Next(0, 10000).ToString()
        ... }
    ```
3. Subscribe router change when app begin
    ```fsharp
    Cmd.batch [
      Cmd.ofSub (fun dispatch ->
        Navigator.subscribe routerId (fun url -> UrlChanged url |> dispatch))

      #if FABLE_COMPILER
      Cmd.ofMsg (UrlChanged "/")
      #endif
    ]
    ```
4. Define routes and use it with `update` function
    ```fsharp
    let routes: Router<State, Cmd<Msg>> =
        choose
          [
            routeCi  ""          (fun state -> { state with CurrentPage = Home "Home" }, Cmd.none)
            routeCi  "/home"     (fun state -> { state with CurrentPage = Home "Home" }, Cmd.none)
            routeCi  "/about"    (fun state -> { state with CurrentPage = About }, Cmd.none)

            subRouteCi "/blog"
              [
                routeCi  ""      (fun state -> { state with CurrentPage = Blog None }, Cmd.none)
                routeCif "/%i"   (fun state id -> { state with CurrentPage = Blog (Some id) }, Cmd.none)
              ]

            routeAny             (fun state url -> { state with CurrentPage = NotFound url }, Cmd.none)
          ]

    let update msg (state: State) =
        match msg with
        | UrlChanged url ->
            match routes state url with
            | Some x -> x
            | None -> { state with CurrentPage = NotFound url }, Cmd.none
    ```
5. Use it in the ui.
    ```fsharp
    let ssrLink routerId link label =
      a 
        [
          OnClick (fun e -> e.preventDefault(); Navigator.newUrl routerId link)
          Href link
        ] 
        [
          str label
        ] 
    ```
6. For server side rendering (SSR) implementation please go to `demo/Server`


# How to run demo
1. Make sure you have `dotnet sdk`, `yarn`, `paket`, `fake-cli` installed globally
2. cd to `./demo` and run `fake build -t RunClient`
3. cd to `./demo` and run `fake build -t RunServer` 