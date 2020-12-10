(*
let a = int_of_string "+123"
let () = print_string (string_of_int a)
*)

let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list =
    String.split_on_char '\n' vsebina

let dobi_ukaz (vrstica : string) : (string * int) =
    let seznam = String.split_on_char ' ' vrstica
    in 
    List.nth seznam 0, int_of_string (List.nth seznam 1)

let prevedi_seznam (sez : string list) : (string * int) list =
    List.map dobi_ukaz sez

let preberi_zadnji_element_seznama (sez : 'a list) : 'a =
    List.nth (List.rev sez) 0

let element_je_v_seznamu (element : 'a) (seznam : 'a list) : bool =
    List.mem element seznam

let poracunaj (seznam_ukazov : (string * int) list) =
    let rec ukazi (seznam_ukazov : (string * int) list) (seznam_dosedanjih_vrstic : int list) (stevec : int) : int =
        let vrsta = (preberi_zadnji_element_seznama seznam_dosedanjih_vrstic)
        in
        let trenutni_ukaz, argument = List.nth seznam_ukazov vrsta
        in
        let stevec' = (
            match trenutni_ukaz with
            | "acc" -> stevec + argument
            | _ -> stevec
        )
        in
        let vrsta' = (
            match trenutni_ukaz with
            | "jmp" -> vrsta + argument
            | _ -> vrsta + 1
        )
        in
        if element_je_v_seznamu vrsta' seznam_dosedanjih_vrstic then
            stevec'
        else
            ukazi seznam_ukazov (seznam_dosedanjih_vrstic @ [vrsta']) stevec'

    in 
    ukazi seznam_ukazov [0] 0


let poracunaj2 (seznam_ukazov : (string * int) list) =
    let rec ukazi2 (seznam_ukazov : (string * int) list) (seznam_dosedanjih_vrstic : int list) (stevec : int) (kontrola : bool): (int * bool) =
        let vrsta = (preberi_zadnji_element_seznama seznam_dosedanjih_vrstic)
        in
        if vrsta >= List.length seznam_ukazov then
            (stevec, true)
        else
            let trenutni_ukaz, argument = List.nth seznam_ukazov vrsta
            in
            let stevec' = (
                match trenutni_ukaz with
                | "acc" -> stevec + argument
                | _ -> stevec
            )
            in
            if element_je_v_seznamu (vrsta + 1) seznam_dosedanjih_vrstic then
                (stevec', false)
            else
                let output = ukazi2 seznam_ukazov (seznam_dosedanjih_vrstic @ [vrsta + (if trenutni_ukaz = "jmp" then argument else 1)]) stevec' kontrola
                in    
                let rezultat = snd output
                in
                if rezultat = true
                    then output
                else
                    if (element_je_v_seznamu (vrsta + argument) seznam_dosedanjih_vrstic) || kontrola = true then
                        (stevec', false)
                    else
                        let output' = ukazi2 seznam_ukazov (seznam_dosedanjih_vrstic @ [vrsta + (if trenutni_ukaz = "nop" then argument else 1)]) stevec' true
                        in
                        output'

    in 
    fst (ukazi2 seznam_ukazov [0] 0 false)


let naloga1 (vsebina : string) : string =
    vsebina |> prevedi_vsebino_datoteke_v_seznam |> prevedi_seznam |> poracunaj |> string_of_int

let naloga2 (vsebina : string) : string =
    vsebina |> prevedi_vsebino_datoteke_v_seznam |> prevedi_seznam |> poracunaj2 |> string_of_int

let () = Sys.chdir "Resitve"
let () = Sys.chdir "8"

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
    let vsebina_datoteke = preberi_datoteko "day_8.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_8_1.out" odgovor1;
    izpisi_datoteko "day_8_2.out" odgovor2
