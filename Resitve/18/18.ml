let prevedi_vsebino_datoteke_v_seznam_stevil (vsebina : string) : string list =
    String.split_on_char '\n' vsebina


let iz_stringa_v_seznam_charov (s : string) : char list =
    let rec pomozna_funkcija (s' : string) (indeks : int) : char list =
        match indeks with
        | a when a = String.length s' -> []
        | _ -> (s'.[indeks] :: (pomozna_funkcija s' (indeks + 1)))
    in
    pomozna_funkcija s 0


(* Funkcija dobljena na StackOverflowu: https://stackoverflow.com/questions/49184057/does-ocaml-have-a-module-that-is-like-isdigit-and-isalpha-in-c-c *)
let is_digit = function '0' .. '9' -> true | _ -> false


let pretvori_char_v_int (znak : char) : int =
    (int_of_char znak) - 48


(* predvidevamo, da so vsa števila enomestna *)
let izracunaj (vrsta : char list) : int =
    let rec pretvori_vrsto (vrsta' : char list) (rezultat : int) (operacija : int -> int -> int) : int * char list =
        match vrsta' with
        | x :: ostanek -> (
            match x with
            | ' ' -> (
                pretvori_vrsto ostanek rezultat operacija
            )
            | x' when is_digit x' -> (
                pretvori_vrsto ostanek (operacija rezultat (pretvori_char_v_int x)) (fun a b -> failwith "Neuporabna funkcija!")
            )
            | '+' -> (
                pretvori_vrsto ostanek rezultat (+)
            )
            | '*' -> (
                pretvori_vrsto ostanek rezultat ( * )
            )
            | '(' -> (
                let oklepaj = pretvori_vrsto ostanek 0 (+)
                in
                pretvori_vrsto (snd oklepaj) (operacija rezultat (fst oklepaj)) (fun a b -> failwith "Neuporabna funkcija!")
            )
            | ')' -> (
                (rezultat, ostanek)
            )
            | _ -> failwith "Neveljaven znak!"
        )
        | [] -> (rezultat, [])
    in
    fst (pretvori_vrsto vrsta 0 (+))


(* predvidevamo, da so vsa števila enomestna *)
let izracunaj2 (vrsta : char list) : int =
    let rec pretvori_vrsto (vrsta' : char list) (rezultat : int) (operacija : int -> int -> int) (st_dodatnih_oklepajev : int) : int * char list =
        match vrsta' with
        | x :: ostanek -> (
            match x with
            | ' ' -> (
                pretvori_vrsto ostanek rezultat operacija st_dodatnih_oklepajev
            )
            | x' when is_digit x' -> (
                pretvori_vrsto ostanek (operacija rezultat (pretvori_char_v_int x)) (fun a b -> failwith "Neuporabna funkcija!") st_dodatnih_oklepajev
            )
            | '+' -> (
                pretvori_vrsto ostanek rezultat (+) st_dodatnih_oklepajev
            )
            | '*' -> (
                pretvori_vrsto ('(' :: ostanek) rezultat ( * ) 1
            )
            | '(' -> (
                let oklepaj = pretvori_vrsto ostanek 0 (+) 0
                in
                if st_dodatnih_oklepajev = 1 then
                    pretvori_vrsto (')' :: snd oklepaj) (operacija rezultat (fst oklepaj)) (fun a b -> failwith "Neuporabna funkcija!") 0
                else
                    pretvori_vrsto (snd oklepaj) (operacija rezultat (fst oklepaj)) (fun a b -> failwith "Neuporabna funkcija!") 0
            )
            | ')' -> (
                (rezultat, ostanek)
            )
            | _ -> failwith "Neveljaven znak!"
        )
        | [] -> (rezultat, [])
    in
    fst (pretvori_vrsto vrsta 0 (+) 0)


(*
let izracunaj2 (vrsta : char list) : int =
    let rec pretvori_vrsto2 (vrsta' : char list) (rezultat : int) (operacija : ((int -> int -> int) * char)) (acc : int) : int * char list =
        match vrsta' with
        | x :: ostanek -> (
            if x = ' ' then
                pretvori_vrsto2 ostanek rezultat operacija acc
            else if is_digit x then
                if snd operacija = '*' then
                    pretvori_vrsto2 ostanek ((fst operacija) rezultat acc) ((fun a b -> failwith "Neuporabna funkcija!"), '?') (pretvori_char_v_int x)
                else if snd operacija = '+' then 
                    pretvori_vrsto2 ostanek rezultat ((fun a b -> failwith "Neuporabna funkcija!"), '?') ((fst operacija) acc (pretvori_char_v_int x))
                else 
                    failwith "To ni veljavna operacija"
            else if x = '+' then
                pretvori_vrsto2 ostanek rezultat ((+), '+') acc
            else if x = '*' then
                pretvori_vrsto2 ostanek rezultat (( * ), '*') acc
            else if x = '(' then
                let oklepaj = pretvori_vrsto2 ostanek 0 ((+), '+') acc
                in
                if snd operacija = '*' then
                    pretvori_vrsto2 (snd oklepaj) ((fst operacija) acc rezultat) ((fun a b -> failwith "Neuporabna funkcija!"), '?') (fst oklepaj)
                else if snd operacija = '+' then
                    pretvori_vrsto2 (snd oklepaj) rezultat ((fun a b -> failwith "Neuporabna funkcija!"), '?') ((fst operacija) (fst oklepaj) acc)
                else 
                    failwith "To ni veljavna operacija"
            else if x = ')' then
                (rezultat, ostanek)
            else failwith "Neveljaven znak!"
        )
        | [] -> ((fst operacija) rezultat acc, [])
    in
    fst (pretvori_vrsto2 vrsta 0 ((+), '+') 0)
*)


let naloga1 (vsebina : string) : string =
    vsebina |> prevedi_vsebino_datoteke_v_seznam_stevil |> List.map iz_stringa_v_seznam_charov |> List.map izracunaj |> List.fold_left (+) 0 |> string_of_int


let naloga2 (vsebina : string) : string =
    vsebina |> prevedi_vsebino_datoteke_v_seznam_stevil |> List.map iz_stringa_v_seznam_charov |> List.map izracunaj2 |> List.fold_left (+) 0 |> string_of_int


let () = Sys.chdir "Resitve"
let () = Sys.chdir "18"


(* Funkcija kopirana iz vzorčnega programa na spletni učilnici *)
let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko "day_18.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_18_1.out" odgovor1;
    izpisi_datoteko "day_18_2.out" odgovor2