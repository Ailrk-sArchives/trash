type name_home = {
  name: string;
  home: string
}

type name_mobile = {
  name: string;
  mobile: string
}

let jotaro = {
  name = "Jotaro";
  home = "123456"
}

let kakyoin = {
  name = "Kakyoin";
  mobile = "114514"
}

let print_name r = print_endline ("Name: " ^ r.name )

let main1 () =
  print_name jotaro;
  print_name kakyoin
  ;;

main1 () ;;
