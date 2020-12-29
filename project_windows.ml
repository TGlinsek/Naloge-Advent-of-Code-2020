(*
- zamenjaj številko v taskih
- dodaj module itd.
- vsc mora biti odprt v AdventOfCodeOcaml (ta globljem izmed dveh)
Le project_windows.ml dela, solution.ml pa ne.
*)


let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina


let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan


module List = struct
  include List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let lines = String.split_on_char '\n'
end


module type Solver = sig
  val naloga1 : string -> string

  val naloga2 : string -> string -> string
end




module Solver0 : Solver = struct
  let cost_fun x = (x / 3) - 2

  let rec full_cost x =
    let c_cost = cost_fun x in
    if c_cost <= 0 then 0 else c_cost + full_cost c_cost

  let naloga1 data =
    let lines = List.lines data in
    lines |> List.int_list
    |> List.fold_left (fun s x -> s + cost_fun x) 0
    |> string_of_int

  let naloga2 data _part1 =
    data |> List.lines |> List.int_list |> List.map full_cost |> List.sum
    |> string_of_int
end


module Solver1 : Solver = struct
  let prevedi_vsebino_datoteke_v_seznam vsebina =
      (* Str.split (vsebina '\n') *)
      String.split_on_char '\n' vsebina  (* char je v enojnih narekovajih *)

  let prevedi_seznam_stringov_v_seznam_intov sez = 
      List.map int_of_string sez

  let pridobi_seznam vsebina_datoteke =
      prevedi_seznam_stringov_v_seznam_intov (
          prevedi_vsebino_datoteke_v_seznam vsebina_datoteke
      )

  let rec vsota (sez : int list) (meja : int) (stevilo_clenov : int) : int list = (
      let rec zapelji_se_po_seznamu (sez' : int list) : int list = (
          match sez' with
          | [] -> [-1]
          | x :: ostanek -> (
              match vsota ostanek (meja - x) (stevilo_clenov - 1) with
              | -1 :: [] -> zapelji_se_po_seznamu ostanek
              | seznam -> x :: seznam
          )
      )
      in
      match meja with
      | 0 ->  (
          match stevilo_clenov with
          | 0 -> []
          | _ -> [-1]
      )
      | presezena_vsota when presezena_vsota < 0 -> [-1]
      | _ -> (zapelji_se_po_seznamu sez)
  )

  let zmnozi_seznam (sez : int list) : string =
      string_of_int (
          List.fold_left ( * ) 1 (
              sez
          )
      )  (* še prevedli bomo produkt v string *)

  let meja = 2020  (* toliko mora znašati vsota členov *)

  let resi_nalogo (vsebina_datoteke : string) (stevilo_clenov : int) : string = 
      zmnozi_seznam (
          vsota (
              pridobi_seznam vsebina_datoteke
          ) meja stevilo_clenov  (* funkcija "vsota" sprejme seznam števil, mejo, in pa število členov *)
      )

  let naloga1 data = resi_nalogo data 2

  let naloga2 data _part1 = resi_nalogo data 3
  
end


module Solver2 : Solver = struct
    let rec iz_stringa_v_seznam_charov (s : string) (indeks : int) = 
        match indeks with
        | a when a = String.length s -> []
        | _ -> s.[indeks] :: (iz_stringa_v_seznam_charov s (indeks + 1))


    let string_v_char_list (s : string) : char list =
        iz_stringa_v_seznam_charov s 0


    (*
    let rec izpisi_seznam_stevil = (function
        | [] -> ()
        | x :: ostanek -> (
            print_char x; print_string " "; izpisi_seznam_stevil ostanek;
        )
    )
    let () = izpisi_seznam_stevil (string_v_char_list "nek string");;
    *)


    let rec stevilo crka niz =
        match niz with
        | x :: ostanek -> (
            (
                if x = crka then
                    1
                else
                    0
            )
            +
            stevilo crka ostanek
        )
        | [] -> 0


    let rec char_na_podanem_mestu niz indeks =
        match niz with
        | [] -> failwith "Indeks presega dolžino seznama!"
        | x :: ostanek when indeks = 0 -> x
        | x :: ostanek -> (
            char_na_podanem_mestu ostanek (indeks - 1)
        )


    let xor (a : bool) (b : bool) =
        (a && (not b)) || ((not a) && b)


    type naloge =
        | Prva
        | Druga


    let rec prestej (sez : (int * int * char * (char list)) list) (naloga : naloge) : int = (
        match sez with
        | x :: ostanek -> ((prestej ostanek naloga) +
            match naloga with
            | Prva -> (
                let min, max, crka, niz = x
                in
                let st = stevilo crka niz
                in
                (
                    if min <= st && st <= max then
                        1
                    else
                        0
                )
            )
            | Druga -> (
                let indeks1, indeks2, crka, niz = x
                in
                if xor 
                    ((char_na_podanem_mestu niz (indeks1 - 1)) = crka) 
                    ((char_na_podanem_mestu niz (indeks2 - 1)) = crka) 
                then
                    1
                else
                    0
            )
        )
        | [] -> 0
    )


    let prevedi_vsebino_datoteke_v_seznam vsebina =
        String.split_on_char '\n' vsebina


    let razdeli_niz_na_stiri_komponente (niz) : int * int * char * (char list) =
        let sez = (String.split_on_char ' ' niz) 
        in
        let a, b, c = (List.nth sez 0, List.nth sez 1, List.nth sez 2)
        in
        let sez2 = String.split_on_char '-' a
        in
        let x, y = (List.nth sez2 0, List.nth sez2 1)
        in
        (int_of_string x, int_of_string y, (b.[0]), string_v_char_list c);;


    (*
    let komponente = razdeli_niz_na_stiri_komponente "17-18 d: ddddddddddddddddzn";;  (* izgleda je treba dodati tole dvojno podpičje *)
    *)


    let delna_funkcija vsebina_datoteke =
        prestej (
            List.map razdeli_niz_na_stiri_komponente (
                prevedi_vsebino_datoteke_v_seznam vsebina_datoteke
            )
        ) 


    let resi_nalogi vsebina_datoteke (naloga : naloge) =
        string_of_int (
            delna_funkcija vsebina_datoteke naloga
        )


    let naloga1 (vsebina_datoteke : string) = 
        resi_nalogi vsebina_datoteke Prva


    let naloga2 (vsebina_datoteke : string) _part1 =
        resi_nalogi vsebina_datoteke Druga
end


module Solver3 : Solver = struct
    let pridobi_seznam vsebina =
        (* Str.split (vsebina '\n') *)
        String.split_on_char '\n' vsebina  (* char je v enojnih narekovajih *)


    let pridobi_znak niz indeks =
        String.get niz indeks


    let dolzina_prve_vrstice sez =
        String.length (List.nth sez 0)


    let rec prestej_drevesa (sez : string list) (odmik : float) (korak : float) : int = (
        match sez with
        | [] -> 0
        | x :: ostanek -> (
            let navzdol_zaokrozen = Float.rem odmik 1.
            in
            (
                if navzdol_zaokrozen <> 0. 
                    then 0
                else 
                    (
                        let celo_stevilo = int_of_float odmik
                        in
                        (
                            if pridobi_znak x (
                                celo_stevilo mod (dolzina_prve_vrstice sez)
                            ) = '#' 
                                then (
                                    1
                                )
                            else 
                                0
                        )
                    )
            ) 
            + 
            (
            prestej_drevesa 
                ostanek 
                (odmik +. korak)
                korak
            )
        )
    )


    let koliko_dreves sez korak =
        prestej_drevesa sez 0. korak


    (*
    let rec prestej_drevesa (sez : string list) (odmik : int) : int = (
        match sez with
        | [] -> 0
        | x :: ostanek -> (
            (if pridobi_znak x (odmik mod (dolzina_prve_vrstice sez)) = '#' then 1 else 0) + (prestej_drevesa ostanek (odmik + 3))
        )
    *)


    let naloga1 (vsebina_datoteke : string) =
        string_of_int (
            koliko_dreves (pridobi_seznam vsebina_datoteke) 
            3.0
        )


    let naloga2 (vsebina_datoteke : string) _part1 = 
        string_of_int (
            let sez = [1.; 3.0; 5.0; 7.0; 0.5]
            in
            let vsebina = pridobi_seznam vsebina_datoteke
            in
            (List.fold_left ( * ) 1 (List.map (koliko_dreves vsebina) sez))
        )
end


module Solver4 : Solver = struct
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
        string_of_int (resi_nalogo vsebina Prva)

    let naloga2 vsebina _part1 =
        string_of_int (Druga |> (resi_nalogo vsebina))
end


module Solver5 : Solver = struct
    let prevedi_vsebino_datoteke_v_seznam vsebina =
        (* Str.split (vsebina '\n') *)
        String.split_on_char '\n' vsebina  (* char je v enojnih narekovajih *)


    let iz_stringa_v_seznam_charov (s : string) : char list =
        let rec pomozna_funkcija (s' : string) (indeks : int) : char list =
            match indeks with
            | a when a = String.length s' -> []
            | _ -> (s'.[indeks] :: (pomozna_funkcija s' (indeks + 1)))
        in
        pomozna_funkcija s 0;;

    let spremeni_sifro_v_binarno (s : string) : bool list =
        let pretvori (x : char) : bool = 
            match x with
            | 'F' -> false  (* 0 *)
            | 'B' -> true  (* 1 *)
            | 'L' -> false
            | 'R' -> true
            | a -> failwith "Neveljavna črka."
        in
        let rec pretvori_seznam_charov (sez : char list) : bool list =
            match sez with
            | [] -> []
            | x :: ostanek -> (pretvori x) :: (pretvori_seznam_charov ostanek)

        in
        (pretvori_seznam_charov (iz_stringa_v_seznam_charov s))


    let rec pretvori_sifre_v_seznamu (sez : string list) : (bool list) list =
        match sez with
        | [] -> []
        | x :: ostanek -> (spremeni_sifro_v_binarno x) :: (pretvori_sifre_v_seznamu ostanek)


    let rec vrni_zadnji_clen_seznama_in_preostanek (sez : 'a list) : ('a list * 'a) =
        match sez with
        | [] -> failwith "Prazen seznam!"
        | x :: [] -> [], x
        | x :: ostanek -> (
            let prvi_del, zadnji_clen = vrni_zadnji_clen_seznama_in_preostanek ostanek
            in
            (x :: prvi_del), zadnji_clen
        )

    let rec spremeni_sifro_v_id (s : bool list) : int =
        match s with
        | [] -> 0
        | x :: ostanek -> (
            let prvi_del, zadnji_clen = vrni_zadnji_clen_seznama_in_preostanek (x :: ostanek)
            in
            2 * (spremeni_sifro_v_id prvi_del) + (
                if zadnji_clen = true then
                    1
                else
                    0
            )
        )

    let rec primerjaj_dve_sifri (sifra : bool list) (sifra2 : bool list) : ((bool list) * (bool list)) =
        match sifra, sifra2 with
        | [], [] -> [], []
        | (x :: ostanek), [] -> failwith "Šifri nista istih dolžin."
        | [], (x :: ostanek) -> failwith "Šifri nista istih dolžin."
        | (x :: ostanek), (x' :: ostanek') -> (
            if x = x' then
                let o, o' = primerjaj_dve_sifri ostanek ostanek'
                in (x :: o), (x :: o')
            else (
                if x = true then
                    (x' :: ostanek', x :: ostanek)  (* levi je minimum, desni maksimum *)
                else 
                    (x :: ostanek, x' :: ostanek')
            )
        )

    let min_sifra =
        let rec pomozna_funkcija' dolzina =
            match dolzina with
            | 0 -> []
            | a -> true :: (pomozna_funkcija' (dolzina - 1))
        in
        pomozna_funkcija' 10


    let maks_sifra =
        let rec pomozna_funkcija'' dolzina =
            match dolzina with
            | 0 -> []
            | a -> false :: (pomozna_funkcija'' (dolzina - 1))
        in
        pomozna_funkcija'' 10


    let rec vrni_min (sez : (bool list) list) : bool list =
        match sez with
        | [] -> min_sifra
        | sifra :: ostanek -> (
            fst (primerjaj_dve_sifri sifra (vrni_min ostanek))
        )


    let rec vrni_maks (sez : (bool list) list) : bool list =
        match sez with
        | [] -> maks_sifra
        | sifra :: ostanek -> (
            snd (primerjaj_dve_sifri sifra (vrni_maks ostanek))
        )

    let rec vsota (sez : (bool list) list) : int =
        match sez with
        | [] -> 0
        | x :: ostanek -> (spremeni_sifro_v_id x) + (vsota ostanek)


    let vsota_aritmeticnega_zaporedja_od_n_do_m (n : int) (m : int) : int =
        ((m + n) * (m - n + 1)) / 2


    let naloga1 (vsebina : string) =
        let seznam_sifer = pretvori_sifre_v_seznamu (prevedi_vsebino_datoteke_v_seznam vsebina)
        in
        string_of_int (spremeni_sifro_v_id (vrni_maks seznam_sifer))

    let naloga2 (vsebina : string) _part1 =
        let seznam_sifer = pretvori_sifre_v_seznamu (prevedi_vsebino_datoteke_v_seznam vsebina)
        in
        let min = spremeni_sifro_v_id (vrni_min seznam_sifer)
        in
        let maks = spremeni_sifro_v_id (vrni_maks seznam_sifer)
        in
        let skupno = vsota seznam_sifer
        in
        string_of_int ((vsota_aritmeticnega_zaporedja_od_n_do_m min maks) - skupno)
end


module Solver8 : Solver = struct
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


    let naloga2 (vsebina : string) (_part1 : string) : string =
        vsebina |> prevedi_vsebino_datoteke_v_seznam |> prevedi_seznam |> poracunaj2 |> string_of_int
end


module Solver9 : Solver = struct
    let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list =
        String.split_on_char '\n' vsebina


    let prevedi_seznam (sez : string list) : int list =
        List.map int_of_string sez


    let vsaj_dva_elementa_sta_enaka (sez : 'a list) : bool =
        let rec pomozna_funkcija (originalen_sez : 'a list) (sez_prejsnjih_elementov : 'a list) : bool =
            match originalen_sez with
            | x :: ostanek -> (List.mem x sez_prejsnjih_elementov) || pomozna_funkcija ostanek (x :: sez_prejsnjih_elementov)
            | [] -> false
        in
        pomozna_funkcija sez []


    (*
    let rec izpisi_seznam_stevil = (function
        | [] -> ()
        | x :: ostanek -> (
            print_int x; print_string " "; izpisi_seznam_stevil ostanek;
        )
    )

    let rec izpisi_seznam_seznamov_stevil = (function
        | [] -> ()
        | sez :: ostanek -> (
            izpisi_seznam_stevil sez; print_string "\n;;;;;"; izpisi_seznam_seznamov_stevil ostanek;
        )
    )
    *)


    let ima_ustrezna_sestevanca ((sez, vsota) : ('a list * int)) : bool =
        if vsaj_dva_elementa_sta_enaka sez then
            failwith "Več enakih elementov!"
        else        
            let predelan_seznam = List.map (fun x -> Stdlib.min x (vsota - x)) sez  (* lahko bi tudi dali max, vseeno je *)
            in
            vsaj_dva_elementa_sta_enaka predelan_seznam


    let rec vrni_prvih_n_elementov_seznama (sez' : 'a list) (n : int) : ('a list * int) =
        match sez' with
        | x :: ostanek when n = 0 -> ([], x)
        | [] -> failwith "Ni dovolj elementov v seznamu."
        | y :: ostanek -> (
            let seznam, st = vrni_prvih_n_elementov_seznama ostanek (n - 1)
            in (y :: seznam, st)
        )


    let rec najdi_stevilo (sez : int list) : int =
        let delni_seznam = vrni_prvih_n_elementov_seznama sez 25
        in
        if ima_ustrezna_sestevanca delni_seznam
            then najdi_stevilo (List.tl sez)
        else 
            snd delni_seznam


    (* ------- Še druga naloga ------- *)


    let rec prvi_prevelik_indeks (sez : int list) (meja : int) : int =
        match sez with
        | [] -> 0
        | x :: ostanek -> (
            if x < meja then 
                1 + prvi_prevelik_indeks ostanek meja 
            else 
                0
        )


    let rec razdeli_sez_na_dva_dela (sez : int list) (indeks_locevanja : int) : (int list * int list) =  (* indeks je začetni indeks drugega seznama *)
        if indeks_locevanja <= 0 then 
            ([], sez) 
        else
            match sez with
            | x :: ostanek -> (
                let prvi, drugi = razdeli_sez_na_dva_dela ostanek (indeks_locevanja - 1)
                in
                (x :: prvi, drugi)
            )
            | [] -> ([], [])


    let rec ustvari_podsezname (sez : int list) (meja : int) : int list list =
        let indeks = prvi_prevelik_indeks sez meja
        in
        match sez with
        | [] -> []
        | x :: ostanek when indeks = 0 -> (
            ustvari_podsezname ostanek meja
        )
        | x :: ostanek -> (
            let prviDel, drugiDel = razdeli_sez_na_dva_dela sez indeks
            in
            prviDel :: (ustvari_podsezname drugiDel meja)
        )


    let rec najdi_vecterico_iz_leve (sez : int list) (meja : int) (zadnji_indeks : int) : int =  (* vrne zadnji indeks *)
        if meja < 0 then 
            -1000 
        else 
            match sez with
            | x :: ostanek -> (
                if meja - x = 0 then zadnji_indeks else
                najdi_vecterico_iz_leve ostanek (meja - x) (zadnji_indeks + 1)
            )
            | [] -> -1000


    let rec najdi_vecterico (sez : int list) (meja : int) (prvi_indeks : int) : (int * int) =  (* vrne (0, -1000), če nič ni bilo najdeno *)
        match sez with
        | x :: ostanek -> (
            let zadnji_indeks = (najdi_vecterico_iz_leve ostanek (meja - x) prvi_indeks + 1)
            in
            if zadnji_indeks <= -1 then
                najdi_vecterico ostanek meja (prvi_indeks + 1)
            else
                (prvi_indeks, zadnji_indeks)
        )
        | [] -> (0, -1000)
    (* par indeksov, ki ga funkcija najdi_vecterico vrne, je oblike (n, m), kjer sta n-ti in m-ti člen še vključena v podmnožico *)


    let rezina (sez : 'a list) (n : int) (m : int) : 'a list =  (* to niso pythonovske rezine, so pa skoraj *)
        if n > m then 
            failwith "Napačni indeksi!" 
        else
            snd (
                razdeli_sez_na_dva_dela (
                    fst (
                        razdeli_sez_na_dva_dela sez (m + 1)
                    )
                ) n
            )


    let rec min_v_seznamu (sez : 'a list) : int =
        match sez with
        | x :: [] -> x
        | x :: ostanek -> Stdlib.min x (min_v_seznamu ostanek)
        | [] -> failwith "Prazen seznam!"


    let rec maks_v_seznamu (sez : 'a list) : int =
        match sez with
        | x :: [] -> x
        | x :: ostanek -> Stdlib.max x (maks_v_seznamu ostanek)
        | [] -> failwith "Prazen seznam!"


    let vrni_min_maks (sez : int list) (meja : int) : (int * int) =
        let a, b = najdi_vecterico sez meja 0
        in
        if b <= -1 then 
            (-1000, -1000) 
        else
            let izsek = rezina sez a b
            in
            let maks = maks_v_seznamu izsek
            in
            let min = min_v_seznamu izsek
            in
            min, maks


    let encryption_weakness (sez : int list) (meja : int) : int =
        let min, maks = vrni_min_maks sez meja
        in
        let vsota = min + maks
        in
        if vsota < 0 then 
            0 
        else 
            vsota


    let rec preisci (podseznami : int list list) (meja : int) : int =
        match podseznami with
        | sez' :: ostanek -> (
            let vrednost = encryption_weakness sez' meja
            in
            if vrednost = 0 then 
                preisci ostanek meja 
            else 
                vrednost
        )
        | [] -> failwith "Ni bilo najdenega zadetka!"


    let razdeli_in_preisci (sez : int list) : int =
        let meja = najdi_stevilo sez  (* izvedemo spet prvo nalogo, saj rabimo rezultat iz prejšnje naloge *)
        in
        let podseznami = ustvari_podsezname sez meja
        in
        preisci podseznami meja


    let naloga1 (vsebina : string) : string =
        vsebina |> prevedi_vsebino_datoteke_v_seznam |> prevedi_seznam |> najdi_stevilo |> string_of_int


    let naloga2 (vsebina : string) (_part1 : string) : string =
        vsebina |> prevedi_vsebino_datoteke_v_seznam |> prevedi_seznam |> razdeli_in_preisci |> string_of_int
end


module Solver11 : Solver = struct
    let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list =
        String.split_on_char '\n' vsebina


    let iz_stringa_v_seznam_charov (s : string) : char list =
        let rec pomozna_funkcija (s' : string) (indeks : int) : char list =
            match indeks with
            | a when a = String.length s' -> []
            | _ -> (s'.[indeks] :: (pomozna_funkcija s' (indeks + 1)))
        in
        pomozna_funkcija s 0


    let sirina_tabele (tabela : char list list) : int =
        (* vzamemo prvo vrstico tabele *)
        (* String.length (List.nth tabela 0) *)
        List.length (List.nth tabela 0)


    let visina_tabele (tabela : char list list) : int =
        List.length tabela


    let preberi_polje_tabele (tabela : char list list) (x : int) (y : int) : char =
        let vrstica = List.nth tabela y
        in
        (* String.get vrstica x *)
        List.nth vrstica x


    let polje_je_zasedeno (tabela : char list list) (x : int) (y : int) : int =
        match x, y with
        | x', _ when x' < 0 || x' >= sirina_tabele tabela -> 0
        | _, y' when y' < 0 || y' >= visina_tabele tabela -> 0
        | x', y' -> if (preberi_polje_tabele tabela x y) = '#' then 1 else 0


    let polje_je_zasedeno_2 (tabela : char list list) (x : int) (y : int) : char =  (* za drugi del naloge *)
        match x, y with
        | x', _ when x' < 0 || x' >= sirina_tabele tabela -> 'L'
        | _, y' when y' < 0 || y' >= visina_tabele tabela -> 'L'
        | x', y' -> preberi_polje_tabele tabela x y


    let rec pomnozi_vrsto_s_clenom vrsta clen = (* iz [x - 1, x, x + 1] * y naredi [(x - 1, y), (x, y), (x + 1, y)] *)
        match vrsta with
        | [] -> []
        | x :: ostanek_x -> (x, clen) :: pomnozi_vrsto_s_clenom ostanek_x clen


    let rec ustvari_produkt_seznamov vrsta stolpec =
        match vrsta, stolpec with
        | sez_x, [] -> []
        | sez_x, y :: ostanek_y -> (pomnozi_vrsto_s_clenom sez_x y) :: ustvari_produkt_seznamov sez_x ostanek_y


    (*
    let curryiranje (f : ('a * 'b) -> 'c) =
        fun a b -> f (a, b)
    *)


    let obratno_curryiranje (f : 'a -> 'b -> 'c) =
        fun (a, b) -> f a b


    let vrni_stevilo_zasedenih_mest_okoli_polja (tabela : char list list) (x : int) (y : int) : int =
        let vrsta = [x - 1; x; x + 1]
        in
        let stolpec = [y - 1; y; y + 1]
        in
        (* [[(x - 1, y - 1), (x, y - 1), ...], [], []]) *)
        let matrika_koordinat = ustvari_produkt_seznamov vrsta stolpec
        in
        (* let () = izpisi_seznam_seznamov_stevil' matrika_koordinat in *)
        let matrika = List.map (fun vrsta -> (List.map (obratno_curryiranje (polje_je_zasedeno tabela)) vrsta)) matrika_koordinat  (* matrika enic in ničel *)
        in
        let stevilo_zasedenih_v_matriki = List.fold_left (+) 0 (List.map (fun vrstica -> List.fold_left (+) 0 vrstica) matrika)
        in
        let x_y_je_zaseden = polje_je_zasedeno tabela x y
        in
        stevilo_zasedenih_v_matriki - x_y_je_zaseden


    let rec v_tej_smeri_se_vidi_sedez (tabela : char list list) (x : int) (y : int) (x' : int) (y' : int) : int =
        match polje_je_zasedeno_2 tabela x y with
        | '#' -> 1
        | 'L' -> 0
        | '.' -> v_tej_smeri_se_vidi_sedez tabela (x + x') (y + y') x' y'
        | _ -> failwith "Neveljaven znak!"


    let osnova = [(1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1)]  (* vse možne smeri *)


    let vrne_stevilo_vidnih_zasedenih_sedezev (tabela : char list list) (x : int) (y : int) : int =
        let funkcija_glede_na_smer = v_tej_smeri_se_vidi_sedez tabela x y
        in
        let seznam_enic_in_nicel = List.map (funkcija_glede_na_smer |> obratno_curryiranje) osnova
        in
        List.fold_left (+) 0 seznam_enic_in_nicel


    let nov_char_v_polju (nacin_stetja_sosedov : char list list -> int -> int -> int) (meja : int) (tabela : char list list) (x : int) (y : int) : char =
        let prejsni_char = preberi_polje_tabele tabela x y
        in
        match prejsni_char with
        | '.' -> '.'
        | _ -> (
            let stevilo_sosedov = nacin_stetja_sosedov tabela x y
            in
            match prejsni_char with
                | 'L' -> (
                    if stevilo_sosedov = 0 then
                        '#'
                    else
                        'L'
                )
                | '#' -> (
                    if stevilo_sosedov >= meja then
                        'L'
                    else
                        '#'
                )
                | _ -> failwith "Neustrezen znak!"
        )


    let naredi_tabelo_indeksov (sirina : int) (visina : int) =
        let rec pomozna_funkcija (sirina' : int) (visina' : int) (zacetni_indeks : int) : (int * int) list list = 
            (* [[(0, 0), (1, 0), ...], [], ...] *)
            let vrstica (dolzina : int) y =
                let rec pomozna_funkcija' (dolzina' : int) y (zacetni_indeks' : int) = 
                    match dolzina' with
                    | 0 -> []
                    | indeks -> (zacetni_indeks', y) :: pomozna_funkcija' (dolzina' - 1) y (zacetni_indeks' + 1)
                in
                pomozna_funkcija' dolzina y 0
            in
            match visina' with
            | 0 -> []
            | indeks -> (vrstica sirina' zacetni_indeks) :: pomozna_funkcija sirina' (visina' - 1) (zacetni_indeks + 1)
        in
        pomozna_funkcija sirina visina 0


    let nova_matrika (nacin_stetja_sosedov : char list list -> int -> int -> int) (meja : int) (tabela : char list list) (sirina : int) (visina : int) tabela_indeksov : char list list =
        List.map (fun vrsta -> List.map (obratno_curryiranje (nov_char_v_polju nacin_stetja_sosedov meja tabela)) vrsta) tabela_indeksov


    let vrni_matriko_ko_se_postopek_konca (nacin_stetja_sosedov : char list list -> int -> int -> int) (meja : int) (tabela : char list list) : char list list =
        let sirina = sirina_tabele tabela
        in
        let visina = visina_tabele tabela
        in
        let tabela_indeksov = naredi_tabelo_indeksov sirina visina
        in
        let rec pomozna (nacin_stetja_sosedov : char list list -> int -> int -> int) (meja : int) (tabela' : char list list) : char list list =
            let nova = nova_matrika nacin_stetja_sosedov meja tabela' sirina visina tabela_indeksov
            in
            match nova with
            | matrika when matrika = tabela' -> tabela'
            | _ -> pomozna nacin_stetja_sosedov meja nova
        in
        pomozna nacin_stetja_sosedov meja tabela


    let prestej_vse_zasedene (tabela : char list list) : int =
        List.fold_left (+) 0 (List.map (fun vrsta -> List.fold_left (+) 0 (List.map (fun clen -> if clen = '#' then 1 else 0) vrsta)) tabela)


    let naloga1 (vsebina : string) : string =
        vsebina |> prevedi_vsebino_datoteke_v_seznam |> (List.map iz_stringa_v_seznam_charov) |> ((vrni_matriko_ko_se_postopek_konca vrni_stevilo_zasedenih_mest_okoli_polja) 4) |> prestej_vse_zasedene |> string_of_int


    (*
    let naloga2 (vsebina : string) (_part1 : string) : string =
        vsebina |> prevedi_vsebino_datoteke_v_seznam |> (List.map iz_stringa_v_seznam_charov) |> (vrni_matriko_ko_se_postopek_konca vrne_stevilo_vidnih_zasedenih_sedezev 5) |> prestej_vse_zasedene |> string_of_int
    *)


    let naloga2 (vsebina : string) (_part1 : string) : string =
        "Ni še rešeno!"
end


module Solver14 : Solver = struct
    let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list =
        String.split_on_char '\n' vsebina


    let iz_stringa_v_seznam_charov (s : string) : char list =
        let rec pomozna_funkcija (s' : string) (indeks : int) : char list =
            match indeks with
            | a when a = String.length s' -> []
            | _ -> (s'.[indeks] :: (pomozna_funkcija s' (indeks + 1)))
        in
        pomozna_funkcija s 0


    type ukazi = 
        | Mask of string
        | Mem of {naslov : string; vrednost : string}


    let vrni_ustrezen_tip (vrstica : string) : ukazi =
        let razdeljena_vrstica = String.split_on_char ' ' vrstica in
        let prvi_del = List.nth razdeljena_vrstica 0 in
        if "mask" = prvi_del then
            Mask (List.nth razdeljena_vrstica 2)
        else  (* mem *)
            let drugi_del_z_zaklepajem = List.nth (String.split_on_char '[' prvi_del) 1 in
            Mem {
                naslov = String.sub drugi_del_z_zaklepajem 0 ((String.length drugi_del_z_zaklepajem) - 1);
                vrednost = List.nth razdeljena_vrstica 2
            }


    let rec element_v_seznamu (element : 'a) (seznam : 'a list) : bool =
        match seznam with
        | [] -> false
        | x :: ostanek -> (x = element) || (element_v_seznamu element ostanek)


    (* v spominu gremo od leve proti desni, saj so na levi najnovejše spremembe, na desni pa najstarejše. Imamo še seznam ponovitev, da vsak naslov vzamemo le enkrat *)
    let prestej (spomin : (string * int) list) : int =
        let rec pomozna_funkcija (spomin : (string * int) list) (seznam_ponovitev : string list) : int =
            match spomin with
            | par :: ostanek -> (
                let naslov = fst par
                in
                let vrednost = snd par
                in
                if not (element_v_seznamu naslov seznam_ponovitev) then  (* naslov ni v seznamu_ponovitev *)
                    vrednost + pomozna_funkcija ostanek (naslov :: seznam_ponovitev)
                else
                    pomozna_funkcija ostanek seznam_ponovitev
            )
            | [] -> 0
        in
        pomozna_funkcija spomin []


    let rec (^) (osnova : int) (potenca : int) : int =
        match potenca with
        | m when m < 0 -> failwith "Ta funkcija ne računa negativnih potenc!"
        | 0 -> 1
        | n -> osnova * (osnova ^ (potenca - 1))


    let preberi_bit (stevilo : int) (mesto : int) : int =  (* najbolj desno je mesto 0 *)
        let potenca = 2 ^ mesto
        in
        ((stevilo - (stevilo mod potenca)) mod (2 * potenca)) / potenca


    let spremeni_vrednost (mask : string) (vrednost : string) : int =  (* vrne decimalno vrednost rezultata *)
        let mask' = iz_stringa_v_seznam_charov mask
        in
        let pretvorjena_vrednost = int_of_string vrednost
        in
        let rec vrni_maskirano_stevilo (maska : char list) (stevilo : int) (mesto : int) : int =
            match maska with
            | znak :: ostanek -> (
                let preostanek = preberi_bit stevilo mesto
                in
                let naslednje_stevilo = (
                    if znak = '0' then
                        stevilo - preostanek * (2 ^ mesto)
                    else if znak = '1' then
                        stevilo + (1 - preostanek) * (2 ^ mesto)
                    else if znak = 'X' then
                        stevilo
                    else 
                        failwith "Neveljaven znak v masku!"
                )
                in
                vrni_maskirano_stevilo ostanek naslednje_stevilo (mesto - 1)
            )
            | [] -> stevilo
        in
        vrni_maskirano_stevilo mask' pretvorjena_vrednost 35


    let zazeni_program (vsebina : string list) =
        let rec pomozna (vsebina : string list) (trenutni_mask : string) (trenutni_spomin : (string * int) list) : int =
            match vsebina with
            | vrstica :: preostanek -> (
                match vrni_ustrezen_tip vrstica with
                | Mask niz -> (pomozna preostanek niz trenutni_spomin)
                | Mem par -> (
                    let naslov = par.naslov
                    in
                    let vrednost = par.vrednost
                    in 
                    let nov_spomin = (naslov, (spremeni_vrednost trenutni_mask vrednost))
                    in
                    pomozna preostanek trenutni_mask (nov_spomin :: trenutni_spomin)  (* novejše vnose dodajamo na levo *)
                )
            )
            | [] -> prestej trenutni_spomin
        in
        pomozna vsebina "Tu je lahko karkoli." []


    let naloga1 (vsebina : string) : string =
        vsebina |> prevedi_vsebino_datoteke_v_seznam |> zazeni_program |> string_of_int
    

    let naloga2 (vsebina : string) (_part1 : string) : string =
        "Ni še rešeno!"
end


module Solver15 : Solver = struct
    let prevedi_vsebino_datoteke_v_seznam_stevil (vsebina : string) : int list =
        (String.split_on_char '\n' vsebina) |> List.map int_of_string


    let rec obrni_seznam (sez : 'a list) : 'a list =  (* seznam obrnemo, saj se elementi dodajajo iz leve in ne iz desne *)
        match sez with
        | [] -> []
        | x :: ostanek -> (obrni_seznam ostanek) @ [x]


    let rec vrni_indeks_ponovitve (sez : int list) (element : int) (stevec : int) : int =
        match sez with
        | x :: ostanek -> if x = element then stevec else vrni_indeks_ponovitve ostanek element (stevec + 1)
        | [] -> 0


    let naslednje_stevilo (sez_rezultatov : int list) : int =
        match sez_rezultatov with
        | x :: ostanek -> vrni_indeks_ponovitve ostanek x 1
        | [] -> failwith "To se ne bi smelo zgoditi."


    let rec napolni_seznam (sez : int list) : int =
        match List.length sez with
        | 2020 -> List.nth sez 0
        | _ -> napolni_seznam ((naslednje_stevilo sez) :: sez)


    let naloga1 (vsebina : string) : string =
        vsebina |> prevedi_vsebino_datoteke_v_seznam_stevil |> obrni_seznam |> napolni_seznam |> string_of_int


    let naloga2 (vsebina : string) (_part1 : string) : string =
        "Ni še rešeno!"
end


module Solver18 : Solver = struct
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


    let naloga1 (vsebina : string) : string =
        vsebina |> prevedi_vsebino_datoteke_v_seznam_stevil |> List.map iz_stringa_v_seznam_charov |> List.map izracunaj |> List.fold_left (+) 0 |> string_of_int


    (*
    let naloga2 (vsebina : string) : string =
        vsebina |> prevedi_vsebino_datoteke_v_seznam_stevil |> List.map iz_stringa_v_seznam_charov |> List.map izracunaj2 |> List.fold_left (+) 0 |> string_of_int
    *)


    let naloga2 (vsebina : string) (_part1 : string) : string =
        "Ni še rešeno!"
end


module Solver22 : Solver = struct
    let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list =
        String.split_on_char '\n' vsebina


    let rec razdeli_sez_na_dva_dela_brez_indeksa (sez : 'a list) (indeks_locevanja : int) : ('a list * 'a list) =  (* indeksti člen se bo izgubil *)
        if indeks_locevanja <= 0 then 
            ([], List.tl sez) 
        else
            match sez with
            | x :: ostanek -> (
                let prvi, drugi = razdeli_sez_na_dva_dela_brez_indeksa ostanek (indeks_locevanja - 1)
                in
                (x :: prvi, drugi)
            )
            | [] -> ([], [])


    let vrni_seznama_stevil (vsebina : string list) : int list * int list =
        let stevilo_vrstic = List.length vsebina
        in
        if stevilo_vrstic mod 2 = 0 then 
            failwith "Sodo število vrstic!"
        else
            let indeks_prazne_vrstice = (stevilo_vrstic - 1) / 2
            in
            let a, b = razdeli_sez_na_dva_dela_brez_indeksa vsebina indeks_prazne_vrstice
            in
            List.map int_of_string (List.tl a), List.map int_of_string (List.tl b)


    let prestej_tocke (kup1 : int list) (kup2 : int list) : int =
        let rec igra (kup1 : int list) (kup2 : int list) : int list =  (* vrne zmagovalčev kup *)
            if kup1 = [] then
                kup2
            else if kup2 = [] then
                kup1
            else
                let karta1 = List.nth kup1 0
                in
                let karta2 = List.nth kup2 0
                in
                let ostanek1 = List.tl kup1
                in
                let ostanek2 = List.tl kup2
                in
                if karta1 > karta2 then  (* izenačenje se ne more zgoditi *)
                    igra (ostanek1 @ [karta1; karta2]) ostanek2
                else
                    igra ostanek1 (ostanek2 @ [karta2; karta1])  (* dejansko vrstni red parametrov ni važen *)
        in
        let zmagovalchev_seznam = igra kup1 kup2
        in
        let dolzina_seznama = List.length zmagovalchev_seznam  (* bi lahko tudi na začetku, ampak ni važno *)
        in
        let prevrednoten_seznam = List.mapi (fun (indeks : int) (el : int) : int -> el * (dolzina_seznama - indeks)) zmagovalchev_seznam
        in
        List.fold_left (+) 0 prevrednoten_seznam


    let obratno_curryiranje (f : 'a -> 'b -> 'c) : ('a * 'b -> 'c) =
        fun (a, b) -> f a b


    let naloga1 (vsebina : string) : string =
        vsebina |> prevedi_vsebino_datoteke_v_seznam |> vrni_seznama_stevil |> (prestej_tocke |> obratno_curryiranje) |> string_of_int


    let naloga2 (vsebina : string) (_part1 : string) : string =
        "Ni še rešeno!"
end


module Solver23 : Solver = struct
    let rec zasuci_seznam_za (seznam : 'a list) (mesta : int) : 'a list =  (* ta prvi element gre na konec *)
        match mesta with
        | 0 -> seznam
        | a when a > 0 -> (
            match seznam with
            | [] -> []
            | x :: ostanek -> zasuci_seznam_za (ostanek @ [x]) (mesta - 1)
        )
        | _ -> failwith "Le nenegativni argumenti so dov"


    let vrni_vecjega_glede_na_primerjalno_funkcijo (f : 'a -> int) (a, a' : 'a * 'b) (b, b' : 'a * 'b) : ('a * 'b) =
        if f a >= f b then
            (a, a')
        else
            (b, b')


    let minimum_parov (a : int * int) (b : int * int) : (int * int) =  (* glede na prvo komponento v paru *)
        vrni_vecjega_glede_na_primerjalno_funkcijo (fun x -> -x) a b


    let maksimum_parov (a : int * int) (b : int * int) : (int * int) =  (* glede na prvo komponento v paru *)
        vrni_vecjega_glede_na_primerjalno_funkcijo (fun x -> x) a b


    let rec vrni_neposredno_manjsega_od_stevila (seznam : int list) (stevilo : int) : (int * int) =  (* če vhodno število najmanjše v seznamu, vrne funkcija največjega *)
        (* levi je vrednost, desni je indeks *)
        match seznam with
        | [] -> failwith "Prazen seznam"
        | x :: [] -> (x, 0)
        | x :: ostanek -> (
            let rezultat = (vrni_neposredno_manjsega_od_stevila ostanek stevilo)
            in
            (* <> pomeni xor *)
            if (x < stevilo) <> (fst rezultat < stevilo) then  (* ni vseeno, ali imamo stroge ali nestroge neenačaje *)
                let min = minimum_parov (x, -1) rezultat
                in
                fst min, (snd min) + 1
            else
                let max = maksimum_parov (x, -1) rezultat
                in
                fst max, (snd max) + 1
        )


    (* Ideja:
    let igra (seznam : int list) (st_rund : int) : int list =
        odstrani el
        odstranjen_sez, ostalo
        vrni_indeks_destinacije
        vstavi_seznam_za_destinacijo
        zasuci_seznam_za_eno
        igra nov_seznam
    *)


    let odstrani_trojico (seznam : int list) : (int list * int list) =  (* levi je trojica, desni preostanek *)
        let f = List.nth seznam
        in
        [f 1; f 2; f 3], (f 0) :: (seznam |> List.tl |> List.tl |> List.tl |> List.tl)


    let rec razdeli_na_dva_dela (indeks : int) (sez : 'a list) : ('a list * 'a list) =  (* indeks je začetni indeks drugega seznama *)
        if indeks <= 0 then 
            ([], sez) 
        else
            match sez with
            | x :: ostanek -> (
                let prvi, drugi = razdeli_na_dva_dela (indeks - 1) ostanek
                in
                (x :: prvi, drugi)
            )
            | [] -> ([], [])


    let rec igra (seznam : int list) (st_rund : int) : int list =
        if st_rund = 0 then
            seznam
        else
            let trojica, ostanek = odstrani_trojico seznam
            in
            let indeks_destinacije = snd (vrni_neposredno_manjsega_od_stevila ostanek (List.nth ostanek 0))
            in
            let prvi_del, drugi_del = razdeli_na_dva_dela (indeks_destinacije + 1) ostanek
            in
            let nov_sez = prvi_del @ (trojica @ drugi_del)
            in
            igra (zasuci_seznam_za nov_sez 1) (st_rund - 1)


    let pretvori_char_v_int (znak : char) : int =
        let vrednost = (int_of_char znak) - 48
        in
        if vrednost < 0 || vrednost > 9 then
            failwith "Tega znaka ne moremo prevesti v int!"
        else
            vrednost


    let iz_stringa_v_seznam_intov (s : string) : int list =
        let rec pomozna_funkcija (s' : string) (indeks : int) : int list =
            match indeks with
            | a when a = String.length s' -> []
            | _ -> (pretvori_char_v_int s'.[indeks]) :: (pomozna_funkcija s' (indeks + 1))
        in
        pomozna_funkcija s 0


    let rec iz_seznama_intov_v_string (sez : int list) : string =
        match sez with
        | [] -> ""
        | x :: ostanek -> String.concat "" [string_of_int x; iz_seznama_intov_v_string ostanek] 


    let stevilo_iger1 = 100


    let naloga1 (vsebina : string) : string =
        let dobljem_seznam = stevilo_iger1 |> (vsebina |> iz_stringa_v_seznam_intov |> igra)
        in
        let indeks_enke = snd (vrni_neposredno_manjsega_od_stevila dobljem_seznam 2)
        in
        let zasukan_seznam = zasuci_seznam_za dobljem_seznam indeks_enke
        in
        iz_seznama_intov_v_string (List.tl zasukan_seznam)


    let milijon = 1000000


    let stevilo_lonckov = milijon


    let rec naredi_seznam_od_n_do_m (n : int) (m : int) =  (* in n in m sta v seznamu*)
        match m - n with
        | 0 -> [n]
        | _ when m > n -> n :: naredi_seznam_od_n_do_m (n + 1) m
        | _ -> failwith "Zgornja meja mora biti večja kot spodnja."


    let pridobi_seznam_z_milijon_cleni (vhodni_seznam : int list) : int list =  (* no, če spremenimo število lončkov, ni milijon *)
        let zacetni_indeks = List.length vhodni_seznam + 1
        in
        vhodni_seznam @ (naredi_seznam_od_n_do_m zacetni_indeks stevilo_lonckov)


    let stevilo_iger2 = 10 * milijon


    (* Stack overflow - morda bi se splačalo to zapisat z repno rekurzijo:
    let naloga2 (vsebina : string) (_part1 : string) : string =
        let dobljem_seznam = stevilo_iger2 |> (vsebina |> iz_stringa_v_seznam_intov |> pridobi_seznam_z_milijon_cleni |> igra)
        in
        let indeks_enke = snd (vrni_neposredno_manjsega_od_stevila dobljem_seznam 2)
        in
        let vrednost = (
            if indeks_enke < stevilo_lonckov - 2 then
                (List.nth dobljem_seznam (indeks_enke + 1)) * (List.nth dobljem_seznam (indeks_enke + 2))
            else
                let zasukan_seznam = zasuci_seznam_za dobljem_seznam 2
                in
                (List.nth zasukan_seznam (indeks_enke + 1)) * (List.nth zasukan_seznam (indeks_enke + 2))
        )
        in
        string_of_int vrednost
    *)

    
    let naloga2 (vsebina : string) (_part1 : string) : string =
        "Ni še rešeno!"
end


module Solver24 : Solver = struct
    let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list = (
        String.split_on_char '\n' vsebina
    )


    let iz_stringa_v_seznam_charov (s : string) : char list =
        let rec pomozna_funkcija (s' : string) (indeks : int) : char list =
            match indeks with
            | a when a = String.length s' -> []
            | _ -> (s'.[indeks] :: (pomozna_funkcija s' (indeks + 1)))
        in
        pomozna_funkcija s 0


    (* ---------------------------------------- *)


    type smeri =
        | E
        | W
        | NW
        | NE
        | SW
        | SE


    let rec pretvori_seznam_znakov_v_seznam_smeri (vrsta : char list) : smeri list = (
        match vrsta with
        | [] -> []
        | 'e' :: xs -> E :: pretvori_seznam_znakov_v_seznam_smeri xs
        | 'w' :: xs -> W :: pretvori_seznam_znakov_v_seznam_smeri xs
        | a :: b :: xs -> (
            (
                match a, b with
                | 'n', 'w' -> NW
                | 'n', 'e' -> NE
                | 's', 'w' -> SW
                | 's', 'e' -> SE
                | _ -> failwith "Neustrezni znaki v seznamu!"
            )
            :: pretvori_seznam_znakov_v_seznam_smeri xs
        )
        | _ -> failwith "Neustrezni znaki v seznamu!"
    )


    (* ---------------------------------------- *)


    type koordinate = {
        x : int;
        y : int
    }


    let pretvori (nabor : int * int) : koordinate =
        {x = (fst nabor); y = (snd nabor)}


    let (++) (a : koordinate) (b : koordinate) : koordinate =
        {x = a.x + b.x; y = a.y + b.y}


    (* os SW-NE predstavlja y os (NE poveča y koordinato), os W-E x os (E poveča x koordinato), SE-WN pa je kombinacija obeh *)
    let rec smeri_priredi_koordinate (pot : smeri list) : koordinate =
        match pot with
        | smer :: xs -> (
            pretvori (
                match smer with
                | E -> (1, 0)
                | W -> (-1, 0)
                | NW -> (-1, 1)
                | NE -> (0, 1)
                | SW -> (0, -1)
                | SE -> (1, -1)
            )
            ++ smeri_priredi_koordinate xs
        )
        | [] -> pretvori (0, 0)


    (* ---------------------------------------- *)


    let prestej_crne_ploscice_v_seznamu_vrstic (seznam : 'a list) : int =  (* če se element seznama ponovi lihokrat, se šteje v skupni seštevek, če sodokrat, pa ne *) 
        let ali_element_v_seznamu (seznam' : 'a list) (el : 'a) : (('a list) option) =  (* le za sezname, v katerih se el ne pojavi več kot enkrat *)
            (* če element je v seznamu, se vrne isti seznam, a brez tistega elementa *)
            let rec pomozna (seznam'' : 'a list) : ('a list * bool) =
                match seznam'' with
                | [] -> ([], false)
                | x :: xs -> (
                    let sez, status = pomozna xs
                    in
                    if x = el then (
                        if status = true then
                            failwith "Seznam ne bi smel imeti dva enaka elementa!"
                        else
                            (sez, true)
                    )
                    else (x :: sez, status)
                )
            in
            let nov_sez, odgovor = pomozna seznam'
            in
            if odgovor = false then
                None
            else
                Some nov_sez
        in
        let prestej (sez' : 'a list) : int =
            let rec pomozna'' (sez'' : 'a list) : ('a list * int) =
                match sez'' with
                | [] -> [], 0
                | x :: xs -> (
                    let delni_seznam, stevec = pomozna'' xs
                    in
                    if stevec < 0 then
                        failwith "Stevec mora vedno biti nenegativen!"
                    else
                        let obdelan_delni_seznam = ali_element_v_seznamu delni_seznam x
                        in
                        match obdelan_delni_seznam with
                        | Some s -> s, stevec - 1
                        | None -> x :: delni_seznam, stevec + 1
                )
            in
            snd (pomozna'' sez')
        in
        prestej seznam


    (* ---------------------------------------- *)


    let naloga1 (vsebina : string) : string =
        vsebina |> prevedi_vsebino_datoteke_v_seznam |> List.map iz_stringa_v_seznam_charov |> List.map pretvori_seznam_znakov_v_seznam_smeri |> List.map smeri_priredi_koordinate |> prestej_crne_ploscice_v_seznamu_vrstic |> string_of_int


    (* ---------------------------------------- *)


    (* S slovarji te funkcije ne bi potrebovali *)
    let rec koliko_ponovitev_elementa_v_seznamu (seznam : 'a list) (el : 'a) : int =  (* če se element le enkrat pojavi, se to šteje kot ena ponovitev *)
        match seznam with
        | x :: xs -> (
            (if x = el then 1 else 0)
            + koliko_ponovitev_elementa_v_seznamu xs el
        )
        | [] -> 0


    let e = pretvori (1, 0)
    let w = pretvori (-1, 0)
    let nw = pretvori (-1, 1)
    let ne = pretvori (0, 1)
    let sw = pretvori (0, -1)
    let se = pretvori (1, -1)


    let vrni_sosede_tocke (tocka : koordinate) : koordinate list =
        let tocka' = pretvori (tocka.x, tocka.y)
        in
        [
            tocka' ++ e;
            tocka' ++ w;
            tocka' ++ nw;
            tocka' ++ ne;
            tocka' ++ sw;
            tocka' ++ se
        ]


    (* V pythonu bi to lahko naredili s slovarji. Ključi so koordinate, vrednosti pa število pojavitev v seznamu_sosedov *)
    let rec seznam_sosedov (seznam_crnih : koordinate list) : koordinate list =  (* seznam_crnih vsebuje samo unikatne elemente *)
        match seznam_crnih with
        | tocka :: xs -> (
            (vrni_sosede_tocke tocka) @ (seznam_sosedov xs)  (* tukaj hočemo s ponovitvami *)
        )
        | [] -> []


    (* Vsi v seznamu_sosedov, ki niso v "seznam"-u, so beli *)
    (* 
    Pogoj iz naloge lahko povemo tako:
    Izberi vse, ki so "sosede natanko dveh črnih" ali "soseda ene do dveh črnih in so črne". Natanko te so črne v naslednji rundi.
    *)
    let primerjaj (seznam : koordinate list) (seznam_sosedov' : koordinate list) : koordinate list =  (* vrne vse, ki bodo črni v naslednji rundi *)
        let rec pomozna_funkcija (sez_sosedov : koordinate list) : (koordinate list * koordinate list) =
            match sez_sosedov with
            | x :: xs -> (
                let stevilo_iksov_v_sosedih = koliko_ponovitev_elementa_v_seznamu seznam_sosedov' x
                in
                let iks_v_seznamu = (koliko_ponovitev_elementa_v_seznamu seznam x) > 0
                in
                let ostanek = pomozna_funkcija xs
                in
                if (koliko_ponovitev_elementa_v_seznamu (snd ostanek) x) > 0 then ostanek
                else
                (
                    if (stevilo_iksov_v_sosedih = 2) || (stevilo_iksov_v_sosedih = 1 && iks_v_seznamu) then 
                        x :: (fst ostanek), x :: (snd ostanek)
                    else
                        fst ostanek, x :: (snd ostanek)
                )
            )
            | [] -> [], []
        in
        fst (pomozna_funkcija seznam_sosedov')


    (* ---------------------------------------- *)


    (*
    let rec odstrani_dvojnike_iz_seznama (seznam : 'a list) : 'a list =
        match seznam with
        | x :: xs -> (
            let iks_v_seznamu = (koliko_ponovitev_elementa_v_seznamu xs x) > 0
            in
            (if not iks_v_seznamu then [x] else []) 
            @ (odstrani_dvojnike_iz_seznama xs)
        )
        | [] -> []
    *)


    let transformacija (trenutni_seznam : koordinate list) : koordinate list =  (* transformacija v enem dnevu *)
        (primerjaj trenutni_seznam (seznam_sosedov trenutni_seznam))  (* na koncu morajo biti le enojni *)


    (* ---------------------------------------- *)


    let rec iz_seznama_odstrani_vse_pojavitve_elementa (seznam : 'a list) (element : 'a) : 'a list =
        match seznam with
        | x :: xs -> (
            (
                if x = element then [] else [x]
            ) 
            @ iz_seznama_odstrani_vse_pojavitve_elementa xs element
        )
        | [] -> []


    let odstrani_sode (seznam : 'a list) : 'a list = (* odstrani vse pojavitve elementov, ki se pojavijo sodokrat *)
        let rec pomozna'' (sez : 'a list) : 'a list =
            match sez with
            | x :: xs -> (
                let iksov_je_sodo = ((koliko_ponovitev_elementa_v_seznamu seznam x) mod 2) = 0  (* oz. (koliko_ponovitev_elementa_v_seznamu xs x) mod 2 = 1 *)
                in
                if iksov_je_sodo then 
                    pomozna'' (iz_seznama_odstrani_vse_pojavitve_elementa xs x)  (* lahko damo tudi sez namesto xs v tem primeru *)
                else 
                    x :: (pomozna'' xs)
            )
            | [] -> []
        in
        pomozna'' seznam


    let rec ponovi_postopek (funkcija : 'a -> 'a) (n : int) (zacetno_stanje : 'a) : 'a =
        if n = 0 then
            zacetno_stanje
        else
            ponovi_postopek funkcija (n - 1) (funkcija zacetno_stanje)


    (* ---------------------------------------- *)


    let naloga2 (vsebina : string) (_part1 : string) : string =
        vsebina |> prevedi_vsebino_datoteke_v_seznam |> List.map iz_stringa_v_seznam_charov |> List.map pretvori_seznam_znakov_v_seznam_smeri |> List.map smeri_priredi_koordinate |> odstrani_sode |> ponovi_postopek transformacija 100 |> List.length |> string_of_int
end


module Solver25 : Solver = struct
    let prvotno_subjektno_stevilo = 7


    let transformacija (a : int) (subjektno_stevilo : int) : int =
        let b = a * subjektno_stevilo
        in
        b mod 20201227


    let pridobi_velikost_zanke (cilj : int) : int =
        let rec pomozna (stevec : int) (zacetek : int) (cilj : int) (meja : int) : int =  (* repna rekurzija: pomnilnik lahko pozabi na prejšnje klice funkcije, ker ne vplivajo na rezultat *)
            if meja = 0 then
                failwith "Infinite loop"
            else
            if zacetek = cilj then
                stevec
            else
                pomozna (stevec + 1) (transformacija zacetek prvotno_subjektno_stevilo) cilj (meja - 1)
        in
        pomozna 0 1 cilj 15000000  (* meja je samo zato, če se slučajno zacikla (ampak se ne) *)


    let uporabi_transformacijo_n_krat (start : int) (n : int) (subjektno_stevilo : int) : int =
        let rec pomozna' (zacetek : int) (indeks : int) : int =  (* spet repna rekurzija*)
            if indeks = 0 then
                zacetek
            else
                pomozna' (transformacija zacetek subjektno_stevilo) (indeks - 1)
        in
        pomozna' start n


    (* ---------------------------------------- *)


    let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list = (
        String.split_on_char '\n' vsebina
    )


    let vrni_vrstici (seznam : 'a list) : ('a * 'a) =
        if not (List.length seznam = 2) then
            failwith "Seznam ni dolžine 2!"
        else
            (List.nth seznam 0, List.nth seznam 1)


    let naloga1 (vsebina : string) : string =
        let javni_kljuc_1, javni_kljuc_2 = vsebina |> prevedi_vsebino_datoteke_v_seznam |> List.map int_of_string |> vrni_vrstici
        in
        let velikost_zanke_1 = pridobi_velikost_zanke javni_kljuc_1
        in
        let velikost_zanke_2 = pridobi_velikost_zanke javni_kljuc_2
        in
        (* Tole dvoje je enako, namreč množenje je komutativno, modulo pa ohranja to (vseeno je, a najprej množimo in potem gledamo ostanek, ali obratno). Skratka, modulo je homomorfizem *)
        let rezultat_1 = uporabi_transformacijo_n_krat 1 velikost_zanke_1 javni_kljuc_2  (* 1 samo zato, ker se postopek pač začne z 1, kot piše v navodilih *)
        in
        let rezultat_2 = uporabi_transformacijo_n_krat 1 velikost_zanke_2 javni_kljuc_1
        in
        if not (rezultat_1 = rezultat_2) then
            failwith "To se ne more zgoditi"
        else
            string_of_int rezultat_1
        (* 
        To troje je vse enako, ampak to ni rešitev:
        let rezultat_3 = uporabi_transformacijo_n_krat javni_kljuc_1 (velikost_zanke_2) 7
        let rezultat_4 = uporabi_transformacijo_n_krat javni_kljuc_2 (velikost_zanke_1) 7
        let rezultat_5 = uporabi_transformacijo_n_krat 1 (velikost_zanke_1 + velikost_zanke_2) 7
        Drugič bolje beri navodila
        *)


    let naloga2 (vsebina : string) (_part1 : string) : string =
        "Zadnji dan nima druge naloge!"
end




(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
  | "4" -> (module Solver4)
  | "5" -> (module Solver5)
  | "8" -> (module Solver8)
  | "9" -> (module Solver9)
  | "11" -> (module Solver11)
  | "14" -> (module Solver14)
  | "15" -> (module Solver15)
  | "18" -> (module Solver18)
  | "22" -> (module Solver22)
  | "23" -> (module Solver23)
  | "24" -> (module Solver24)
  | "25" -> (module Solver25)
  | _ -> failwith "Ni še rešeno"


let main () =
  let day = Sys.argv.(1) in
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data = preberi_datoteko ("data/day_" ^ day ^ ".in") in
  let p1_start = Sys.time () in
  let part1 = Solver.naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
  izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
  ()


let _ = main ()
