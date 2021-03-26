module Fun.LightForm.NestFrom


let [<Literal>] Spliter = "."

let (<.>) name1 name2 = name1 + Spliter + name2 
