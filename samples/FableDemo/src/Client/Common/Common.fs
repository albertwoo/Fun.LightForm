[<AutoOpen>]
module Client.Common.Common

open Fable.Core


let inline clientServerExec clientFn serverFn input =
#if FABLE_COMPILER
    clientFn input
#else
    serverFn input
#endif

let inline clientExec clientFn input = clientServerExec clientFn ignore input



[<Emit("decodeURIComponent(escape(window.atob($0)))")>]
let fromBase64 (str: string): string = jsNative

[<Emit("window.btoa(unescape(encodeURIComponent($0)))")>]
let toBase64 (str: string): string = jsNative


[<Emit("navigator.userAgent.indexOf('Trident/7.0') !== -1")>]
let isIE (): bool = jsNative
