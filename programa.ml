class senha_tipo (tipo:string) minutos =
	object
		method get_tipo = tipo
		method get_tempo_atendimento = minutos * 60
	end;;

let senha_A = new senha_tipo "A" 2;;
let senha_B = new senha_tipo "B" 2;;
let senha_C = new senha_tipo "C" 5;;
let senha_D = new senha_tipo "D" 5;;

let tipos_senha = [
	senha_A;
	senha_B;
	senha_C;
	senha_D;
];;

class senha (tipo:senha_tipo) numero hora_tirada =
	object
		val mutable hora_atendimento_senha = 0
		val mutable tempo_atendimento_senha = 0
		val mutable hora_despachada_senha = 0

		method get_tipo = tipo
		method set_hora_atendimento hora_atendimento =
			hora_atendimento_senha <- hora_atendimento;
			tempo_atendimento_senha <- hora_atendimento_senha - hora_tirada;
			hora_despachada_senha <- hora_atendimento_senha + tipo#get_tempo_atendimento
		method get_hora_tirada = hora_tirada
		method get_hora_atendimento = hora_atendimento_senha
		method get_tempo_atendimento = tempo_atendimento_senha
		method get_hora_despachada = hora_despachada_senha
		method to_string = Printf.sprintf "%s %03d" tipo#get_tipo numero
	end;;

let senha_tipo_null = new senha_tipo "NULL" 0;;
let senha_null = new senha senha_tipo_null 0 0;;
class balcao nome fila tipos_senha =
	object (self)
		val mutable senha_atendida = senha_null

		method atende_senha_tipo (t1:senha_tipo) =
			let atende t2 = (t2#get_tipo = t1#get_tipo) in
			List.exists atende tipos_senha
		method atualiza hora =
			if senha_atendida = senha_null || hora >= senha_atendida#get_hora_despachada then
				senha_atendida <- fila#get_proxima_senha self
		method to_string = Printf.sprintf "%s" nome
	end;;

let segundos_to_hms tempo =
	let horas = tempo / (60 * 60) in
	let minutos = (tempo / 60) mod 60 in
	let segundos = tempo mod 60 in
	Printf.sprintf "%2d:%02d:%02d" horas minutos segundos;;

(* http://langref.org/ocaml/lists/modification/remove-by-index *)
let rec del i l =
	match l with
	| [] -> []
	| h::t when i = 0 -> t
	| h::t -> h :: del (i-1) t;;

exception Return of senha;;
let maximo_senhas_espera = 3;;
let balcoes = [];;
class fila =
	let numero_senhas_espera = Hashtbl.create 4 in
	object
		val mutable senhas = []
		val mutable hora_atual = 0
		val mutable tempo_atendimento = 0
		val mutable senhas_atendidas = 0

		method adiciona_senha senha =
			senhas <- senhas @ [senha];
			let n = try Hashtbl.find numero_senhas_espera senha#get_tipo with Not_found -> 0 in
			Hashtbl.replace numero_senhas_espera senha#get_tipo (n + 1)
		method get_proxima_senha (balcao:balcao) =
			try
			let tipos = tipos_senha in
			let loop_senhas i senha =
				let tipo = senha#get_tipo in
				let senhas_em_espera = try Hashtbl.find numero_senhas_espera tipo with Not_found -> 0 in
				if senhas_em_espera > maximo_senhas_espera || balcao#atende_senha_tipo tipo then
				begin
					Hashtbl.replace numero_senhas_espera tipo (senhas_em_espera - 1);
					senhas_atendidas <- senhas_atendidas + 1;
					senha#set_hora_atendimento hora_atual;
					tempo_atendimento <- tempo_atendimento + senha#get_tempo_atendimento;
					senhas <- del i senhas;
					Printf.printf "%s %s %s" (segundos_to_hms hora_atual) senha#to_string balcao#to_string;
					raise (Return senha)
				end
				else
				begin
					let tipos = List.filter (fun tipo -> tipo <> tipo) tipos in
					if tipos = [] then raise (Return senha_null)
				end
			in
			List.iteri loop_senhas senhas;
			raise (Return senha_null)
			with Return s -> s
		method avanca_simulacao hora =
			if hora_atual = 0 then
				hora_atual <- hora;
			if hora < hora_atual then
				Printf.printf "Erro na hora, é anterior à última introduzida\n";
			while hora_atual < hora do
				let loop_balcoes balcao = balcao#atualiza hora_atual in
				List.iter loop_balcoes balcoes;
				if hora_atual < hora then
					hora_atual <- hora_atual + 1
				else
				begin
					if (List.length senhas) = 0 then
						hora_atual <- hora
				end
			done
		method get_fila = senhas
		method calcula_media =
			if senhas_atendidas <> 0 then
				tempo_atendimento / senhas_atendidas
			else
				0
		method to_string =
			let str = Printf.sprintf "%s Senhas em espera: %d" (segundos_to_hms hora_atual) (List.length senhas) in
			if (List.length senhas) <> 0 then
			begin
				let senha s = s#to_string in 
				let lista_senhas = List.map senha senhas in
				Printf.sprintf "%s -> %s" str (String.concat ", " lista_senhas)
			end
			else
				Printf.sprintf "%s\n" str
	end;;

#load "str.cma";;

let letras_senhas = Hashtbl.create 4;;
let put_letras_senhas letra senha_tipo = Hashtbl.replace letras_senhas letra senha_tipo;;

put_letras_senhas "A" senha_A;;
put_letras_senhas "B" senha_B;;
put_letras_senhas "C" senha_C;;
put_letras_senhas "D" senha_D;;

let fila = new fila;;

let balcoes = [
	new balcao "Balcão 1" fila [ senha_A; senha_B ];
	new balcao "Balcão 2" fila [ senha_C ];
	new balcao "Balcão 3" fila [ senha_D ]
];;

class programa =
	let regex = Str.regexp_case_fold "sair\|\([0-2]?[0-9]\):\([0-5][0-9]\):\([0-5][0-9]\) \([A-Z][0-9]\|fila\|media\)" in
	let hms_to_segundos hora minutos segundos = (hora * 60 * 60) + (minutos * 60) + segundos in
	object
		val mutable hora_atual = 0
		val mutable comando = ""

		method ler_e_processar_comandos =
			while comando <> "sair" do
				let linha = String.trim (read_line()) in
				if (Str.string_match regex linha 0) = true then
				begin
					comando <- try (Str.matched_group 4 linha) with Not_found -> "sair";
					if comando <> "sair" then
					begin
						let hora = int_of_string (Str.matched_group 1 linha) in
						let minutos = int_of_string (Str.matched_group 2 linha) in
						let segundos = int_of_string (Str.matched_group 3 linha) in
						hora_atual <- hms_to_segundos hora minutos segundos;
						fila#avanca_simulacao hora_atual;
						if comando = "fila" then
						begin
							Printf.printf "%s" fila#to_string;
							""
						end
						else if comando = "media" then
						begin
							Printf.printf "%s Tempo médio de espera: %s\n" (segundos_to_hms hora_atual) (segundos_to_hms fila#calcula_media);
							""
						end
						else
						begin
							try (
								let senha_tipo = Hashtbl.find letras_senhas (String.sub comando 0 1) in
								let numero = int_of_string (String.sub comando 1 3) in
								let senha_tirada = new senha senha_tipo numero hora_atual in
								fila#adiciona_senha senha_tirada;
								Printf.printf "%s senha %s\n" (segundos_to_hms hora_atual) (senha_tirada#to_string); ""
							) with Not_found -> Printf.printf "Tipo de senha não reconhecido\n"; ""
						end
					end
					else
						exit 0
				end
				else
					Printf.printf "Entrada não reconhecida, introduza 'hh:mm:ss comando' ou 'sair'\n"
			done
	end;;

new programa#ler_e_processar_comandos;;