let razcepi_na_kljuc_in_vrednost (niz : string) : string * string =
    let sez = String.split_on_char ':' niz
    in
    let (a, b) = (List.nth sez 0, List.nth sez 1)
    in
    (a, b);;

let rec obdelaj_podatke (sez : string list) : (((string * string) list) list) =
    (* sprejme seznam vrstic (nizov), vrne seznam setov *)
    match sez with
    | [] -> [[]]
    | x :: ostanek -> (
        match x with
        | "" -> [] :: (obdelaj_podatke ostanek)
        | neprazna -> (
            match obdelaj_podatke ostanek with
            | y :: ostanek -> (
                ((List.map razcepi_na_kljuc_in_vrednost (String.split_on_char ' ' neprazna)) @ y) :: ostanek
            )   (* (obdelaj_podatke ostanek)[0].append_na_začetek(neprazna) *)
            | [] -> failwith "napaka. To se naj ne bi zgodilo, saj vedno začnemo z [[]], nikoli ne more bit torej []"
        )
    )

(*
let a = obdelaj_podatke ["ar:gdr"; "sthfst:hfh"; ""; "tshf:dth"; "dsthd:tht rfgrg:asefas"; "dshh:d"; ""; "a:t"; ""; ""; "sg:rsgs"]
*)

(*
let rec izpisi_seznam_parov (sez : ((string * string) list)) = (
    match sez with
    | [] -> ()
    | (a, b) :: ostanek -> 
        print_string a; print_string "_"; print_string b; print_string "-"; izpisi_seznam_parov ostanek;
)

let rec izpisi_seznam (sez : (((string * string) list) list)) = (
    match sez with
    | [] -> ()
    | x :: ostanek -> (
        izpisi_seznam_parov x; print_string " "; izpisi_seznam ostanek;
    )
)
let () = izpisi_seznam (obdelaj_podatke ["ecl:grn"; "cid:315 iyr:2012 hgt:192cm eyr:2023 pid:873355140 byr:1925 hcl:#cb2c03"; ""; "byr:2027 hcl:ec0cfd ecl:blu cid:120"; "eyr:1937 pid:106018766 iyr:2010 hgt:154cm"]);;
*)

let rec vsebuje_cid (sez : (string * string) list) : bool =
    match sez with
    | [] -> false
    | (a, _) :: ostanek -> (
        (a = "cid") || vsebuje_cid ostanek
    )

(* Funkcija dobljena na StackOverflowu: https://stackoverflow.com/questions/49184057/does-ocaml-have-a-module-that-is-like-isdigit-and-isalpha-in-c-c *)
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let je_alfanumericni_znak (znak : char) =
    is_alpha znak || is_digit znak

let rec niz_je_alfanumericen (niz : (char list)) = (
    match niz with
    | [] -> true
    | x :: ostanek -> (
        (je_alfanumericni_znak x) && niz_je_alfanumericen ostanek
    )
)


let rec niz_je_stevilo (niz : (char list)) = (
    match niz with
    | [] -> true
    | x :: ostanek -> (
        (is_digit x) && niz_je_stevilo ostanek
    )
)

let rec iz_stringa_v_seznam_charov (s : string) (indeks : int) = 
    match indeks with
    | a when a = String.length s -> []
    | _ -> s.[indeks] :: (iz_stringa_v_seznam_charov s (indeks + 1))


let rec veljaven (sez : (string * string) list) : bool =
    match sez with
    | [] -> true
    | (a, b) :: ostanek -> (
        (
            match a with
            | "byr" -> (
                if not (niz_je_stevilo (iz_stringa_v_seznam_charov b 0)) then
                    false
                else
                    let st = int_of_string b
                    in
                    ((String.length b) = 4 && st >= 1920 && st <= 2002)
            )
            | "iyr" -> (
                if not (niz_je_stevilo (iz_stringa_v_seznam_charov b 0)) then

                    false
                else
                    let st = int_of_string b
                    in
                    ((String.length b) = 4 && st >= 2010 && st <= 2020)
            )
            | "eyr" -> (
                if not (niz_je_stevilo (iz_stringa_v_seznam_charov b 0)) then
                    false
                else
                    let st = int_of_string b
                    in
                    ((String.length b) = 4 && st >= 2020 && st <= 2030)
            )
            | "hgt" -> (
                if String.length b <= 2 then
                    false
                else
                    let enota = String.sub b ((String.length b) - 2) 2
                    in
                    let kolicina = String.sub b 0 ((String.length b) - 2)
                    in
                    if not (niz_je_stevilo (iz_stringa_v_seznam_charov kolicina 0)) then
                        false
                    else (
                        let st = int_of_string kolicina
                        in
                        match enota with
                        | "cm" -> (
                            (st >= 150 && st <= 193)
                        )
                        | "in" -> (
                            (st >= 59 && st <= 76)
                        )
                        | _ -> (
                            false
                        )
                )
            )
            | "hcl" -> (
                let prvi_znak = b.[0]
                in
                let stevilski_del = String.sub b 1 ((String.length b) - 1)
                in
                if (String.length stevilski_del) = 6 then
                    (prvi_znak = '#' && niz_je_alfanumericen (iz_stringa_v_seznam_charov stevilski_del 0))
                else
                    false
            )
            | "ecl" -> (
                let sfda = List.mem b ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
                in sfda
            )
            | "pid" -> (
                (String.length b = 9)
                &&
                (
                    niz_je_stevilo (iz_stringa_v_seznam_charov b 0)
                )   
            )
            | "cid" -> true
            | _ -> failwith "Neveljavna koda"
        )
        &&
        veljaven ostanek
    )


type naloge =
    | Prva
    | Druga


let rec prestej_veljavne (sez : ((string * string) list list)) (naloga : naloge) =
    match sez with
    | [] -> 0
    | x :: ostanek -> (
        (
            if (List.length x = 8) || ((List.length x = 7) && not (vsebuje_cid x)) then
                match naloga with
                | Prva -> 1
                | Druga -> (
                    if veljaven x then
                        1
                    else
                    0
                )
            else
                0
        )
        +
        prestej_veljavne ostanek naloga
    )

let prevedi_vsebino_datoteke_v_seznam vsebina =
    String.split_on_char '\n' vsebina

let resi_nalogo vsebina =
    vsebina |> prevedi_vsebino_datoteke_v_seznam |> obdelaj_podatke |> prestej_veljavne;;

let naloga1 vsebina =
    string_of_int (resi_nalogo vsebina Prva);;

let naloga2 vsebina =
    string_of_int (Druga |> (resi_nalogo vsebina));;

Sys.chdir "Naloge-Advent-of-Code-2020";;
Sys.chdir "Resitve";;
Sys.chdir "4";;


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
    let vsebina_datoteke = preberi_datoteko "day_4.in" in
    (* print_string vsebina_datoteke; *)
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_4_1.out" odgovor1;
    izpisi_datoteko "day_4_2.out" odgovor2