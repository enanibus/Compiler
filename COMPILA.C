#include    <stdio.h>
#include    <alloc.h>
#define     CAR_MAX     15
#define     MAX_TS      50
#define     MAX_PS      1000
#define     MAX_ESTRUC  30
#define     MAX_PP      100
#define     MAX_CI      200
#define     MAX_REGLAS  120
#define     DIM         70
#define     MAX_SWITCH  5
#define	    MAX_STRINGS 20
#define     FALSE 	0
#define     TRUE 	1

char r[DIM][DIM]; /* guarda la matriz de relaciones entre s¡mbolos */

static char identificador[CAR_MAX];
int     c, pila = 1;
int     error_lexico = FALSE;

struct  {   int p_token;
            int p_indice; }  ps[MAX_PS];    /* pila del escaner */

struct {    char nombre[CAR_MAX];
            int tipo;
            int bloque;             } ts[MAX_TS];   /* tabla de simbolos */

struct {    char nombre[CAR_MAX];
            int desplazamiento;
            int tipo;
            int profundidad;    } tabla_est[MAX_ESTRUC]; /* tabla de simbolos de estructuras */

/* para leer las cadenas de caracteres del printf */
int ns = 0, num_strings = 0;
char *tabla_strings[MAX_STRINGS];

/* estas se utilizan en la generaci¢n de c¢digo para switch anidados
   Las pongo aqu¡ para inicializarlas en inicializar()              */
int prof_sw[MAX_SWITCH];
int prsw = 0, numcomp = 0, nro_sw = 0;


FILE *fd2;    /* fichero de salida del c¢digo objeto */

main()
{
    fd2 = fopen ("objeto","w");

    inicializar();
    escaner();
    reservar_punt();
    if (!error_lexico) {
	parser();
	hacer_printf();
	reservar_mem();
    }


}

reservar_punt()
{ int i, np = 0;

  for (i = 0; i < MAX_TS; i++)
	if ( ts[i].tipo >= 100 )
		ts[i].tipo = ts[i].tipo - 70;
	else if ( ts[i].tipo == 20 ){
		 fprintf (fd2, "LDA,i __punt%d\n",np);
		 np++;
		 fprintf (fd2, "STA %s\n",ts[i].nombre);
	    }
  fprintf(fd2,"J main\n\n");
}



/* maximo numero de variables temporales. Se usa en la generacio de codigo
  pero la pongo aqui porque la uso en reservar_men    */
int maxi=0;

reservar_mem()
{   int i, np = 0, aux, reserva = 1;

    for (i = 0; i < MAX_TS; i++) {
	if ( ts[i].tipo == 2 || ts[i].tipo == 3 || ts[i].tipo == 8)
		fprintf (fd2, "%s: DC 0\n",ts[i].nombre);
	else if ( ts[i].tipo == 20 ) {
		fprintf (fd2, "%s : DC 0\n",ts[i].nombre);
		fprintf (fd2, "__punt%d : DC 0\n",np);
		np++;
	     }
	     else if ( ts[i].tipo >= 30 && ts[i].bloque != 0
			|| ts[i].tipo <= -30 )  {
			if (ts[i].tipo >= 30)
			  aux = ts[i].tipo - 30;
			else aux = -ts[i].tipo - 30;
			while (tabla_est[aux].desplazamiento != -1) {
				if (tabla_est[aux].tipo != 4)
					reserva++;
				aux++;
			}
			fprintf(fd2, "%s :DS %d\n",ts[i].nombre,reserva-1);
		 }
    }
    /* temporales */
    for (i = 0; i < maxi; i++)
	fprintf (fd2, "__T%d : DC 0\n",i);
    fprintf (fd2,"dir_retorno : DC 0\n");
    /* para swtich */
    i = 0;
    while ( prof_sw[i] > 0 ) {
	fprintf (fd2, "__comp%d: DC 0\n",i);
	i++;
    }
    /* para el printf */
    for (i = 0; i < num_strings; i++)
	   fprintf (fd2, "__s%d : DFSTR \"%s\"\n",i,tabla_strings[i]);
    fprintf (fd2, "__pfs : DC 0\n");
    fprintf (fd2, "__argpf : DC 0\n");
    fprintf (fd2,"END\n");
}




inicializar ()
{   int i, j;

    /* El primer caracter de la pila es $ */
    ps[0].p_token = 15;
    ps[0].p_indice = -1;
    /* Inicializar la pila del scaner */
    for (i = 1; i < MAX_PS; i++) {
        ps[i].p_token = -1;
        ps[i].p_indice = -1;
    }

    /* Inicializar la tabla de simbolos */
    for (i = 0; i < MAX_TS; i++) {
        ts[i].tipo = -1;
        ts[i].bloque = -1;
        for (j = 0; j < CAR_MAX; j++)
            ts[i].nombre[j] = '\0';
    }

    /* Inicializar la tabla de simbolos de estructuras */
    for (i = 0; i < MAX_ESTRUC; i++) {
        tabla_est[i].tipo = -1;
        tabla_est[i].desplazamiento = -1;
        tabla_est[i].profundidad = -1;
        for (j = 0; j < CAR_MAX; j++)
            tabla_est[i].nombre[j] = '\0';
    }


    for (i = 0; i < MAX_SWITCH; i++)
        prof_sw[i] = 0;


    /* Meter las palabras clave en la tabla de simbolos */
    i = insertar (7, "struct");
    ts[i].tipo = 26;
    ts[i].bloque = 0;

    i = insertar (4, "int");
    ts[i].tipo = 29;
    ts[i].bloque = 0;

    i = insertar (5, "char");
    ts[i].tipo = 30;
    ts[i].bloque = 0;

    i = insertar (7, "return");
    ts[i].tipo = 62;
    ts[i].bloque = 0;

    i = insertar (7, "switch");
    ts[i].tipo = 65;
    ts[i].bloque = 0;

    i = insertar (6, "break");
    ts[i].tipo = 66;
    ts[i].bloque = 0;

    i = insertar (5, "case");
    ts[i].tipo = 68;
    ts[i].bloque = 0;

    i = insertar (8, "default");
    ts[i].tipo = 69;
    ts[i].bloque = 0;

    i = insertar (7, "printf");
    ts[i].tipo = 0;
    ts[i].bloque = 1;

    for (j = 0; j < CAR_MAX; j++)
        identificador[j] = '\0';

}



escaner()
{   FILE    *fd;
    int     num = 0, esta = FALSE, indice = 0, punt = -1, estado = 0, token = -1;
    char    *paux;

    fd = fopen ("fuente", "r");

    c = fgetc (fd);
    while (c != EOF && !kbhit()) {
        token = 0;
        switch (estado) {
	    case 0 :  while ( ((c == ' ') || (c == '\n') || (c == 10) ||(c == '\t')) && (c != EOF) )
                                c = fgetc(fd);
                      estado = 1;
                      break;

	    case 1 :  if (letra()) {
                        estado = 2;
                        break;
                      }
                      else if (digito()) {
                                estado = 4;
                                break;
                            }
                            else switch (c) {
                                    case '+' :  estado = 6;
                                                break;
                                    case '/' :  estado = 9;
                                                break;
                                    case '=' :  estado = 11;
                                                break;
                                    case '<' :  estado = 13;
                                                break;
                                    case '>' :  estado = 15;
                                                break;
                                    case '!' :  estado = 17;
                                                break;
                                    case '%' :  estado = 19;
                                                break;
                                    case '|' :  estado = 21;
                                                break;
                                    case '&' :  estado = 23;
                                                break;
                                    case '-' :  estado = 25;
                                                break;
                                    case '*' :  estado = 27;
                                                break;
                                    case ';' :  apilar (37, -1);
                                                estado = 0;
                                                c = fgetc (fd);
                                                break;
                                    case ',' :  apilar (56, -1);
                                                estado = 0;
                                                c = fgetc (fd);
                                                break;
                                    case '(' :  apilar (57, -1);
                                                estado = 0;
                                                c = fgetc (fd);
                                                break;
                                    case ')' :  apilar (58, -1);
                                                estado = 0;
                                                c = fgetc (fd);
                                                break;
                                    case '{' :  apilar (59, -1);
                                                estado = 0;
                                                c = fgetc (fd);
                                                break;
                                    case '}' :  apilar (60, -1);
                                                estado = 0;
                                                c = fgetc (fd);
                                                break;
                                    case '.' :  apilar (64, -1);
                                                estado = 0;
                                                c = fgetc (fd);
                                                break;
                                    case ':' :  apilar (67, -1);
                                                estado = 0;
                                                c = fgetc (fd);
						break;
				    case '"' :  apilar (34, num_strings);
						estado = 29;
						break;
				    case '\'':  c = fgetc (fd);
						apilar (33, c);
						c = fgetc (fd);
						if ( c != '\'' )
							error (1);
						c = fgetc (fd);
						estado = 0;
						break;
				    default  :  break;

                                }
                      break;

            case 2 :  while (letra () || digito ()) {
                        formar (indice);
                        indice++;
                        c = fgetc (fd);
                      }
                      estado = 3;

            case 3 :  token = 27;
                      esta = ver (indice,identificador);
                      if (esta <= -1) {
                        /* El identificador no esta en la TS */
			punt = insertar (indice, identificador);
                        apilar (token, punt);
                      }
		      else {
			if (ts[esta].bloque == 0)
			     reservada (&token, indice);
                        apilar (token, esta);
                      }
                      for (indice = 0; indice < CAR_MAX; indice++)
                            identificador[indice] = '\0';
                      estado = indice = 0;
                      esta = FALSE;
                      break;

            case 4 :  while (digito()) {
                        c = c - '0';
                        num = 10 * num + c;
                        c = fgetc (fd);
                      }
                      estado = 5;

            case 5 :  token = 32;
                      apilar (token, num);
                      estado = num = 0;
                      break;

            case 6 :  c = fgetc (fd);
                      if ( c == '+' )
                        estado = 7;
                      else if ( c == '=') {
                                estado = 8;
                                break;
                           }
                           else {
                            token = 39;
                            apilar (token, -1);
                            estado = 0;
                            break;
                           }

            case 7 :  token = 35;
                      apilar (token, -1);
                      estado = 0;
                      c = fgetc(fd);
                      break;

            case 8 :  token = 51;
                      apilar (token, -1);
                      estado = 0;
                      c = fgetc(fd);
                      break;

            case 9 :  c = fgetc (fd);
                      if ( c == '=')
                         estado = 10;
                      else {
                        token = 41;
                        apilar (token, -1);
                        estado = 0;
                        break;
                      }

            case 10 : token = 54;
                      apilar (token, -1);
                      estado = 0;
                      c = fgetc(fd);
                      break;

            case 11 : c = fgetc (fd);
                      if ( c == '=')
                         estado = 12;
                      else {
                        token = 31;
                        apilar (token, -1);
                        estado = 0;
                        break;
                      }
            case 12 : token = 48;
                      apilar (token, -1);
                      estado = 0;
                      c = fgetc(fd);
                      break;
            case 13 : c = fgetc (fd);
                      if ( c == '=')
                         estado = 14;
                      else {
                        token = 44;
                        apilar (token, -1);
                        estado = 0;
                        break;
                      }
            case 14 : token = 46;
                      apilar (token, -1);
                      estado = 0;
                      c = fgetc(fd);
                      break;  
            
            case 15 : c = fgetc (fd);
                      if ( c == '=')
                         estado = 16;
                      else {
                        token = 43;
                        apilar (token, -1);
                        estado = 0;
                        break;
                      }
            case 16 : token = 45;
                      apilar (token, -1);
                      estado = 0;
                      c = fgetc(fd);
                      break;

            case 17 : c = fgetc (fd);
                      if ( c == '=')
                         estado = 18;
                      else {
                        token = 61;
                        apilar (token, -1);
                        estado = 0;
                        break;
                      }
            case 18 : token = 47;
                      apilar (token, -1);
                      estado = 0;
                      c = fgetc(fd);
                      break;
            case 19 : c = fgetc (fd);
                      if ( c == '=')
                         estado = 20;
                      else {
                        token = 42;
                        apilar (token, -1);
                        estado = 0;
                        break;
                      }
            case 20 : token = 55;
                      apilar (token, -1);                                   
                      estado = 0;
                      c = fgetc(fd);
                      break;
            case 21 : c = fgetc (fd);
                      if ( c == '|')
                         estado = 22;
                      else {
                        error (1);
                        estado = 0;
                        break;
                      }
            case 22 : token = 49;
                      apilar (token, -1);
                      estado = 0;
                      c = fgetc(fd);
                      break;
            case 23 : c = fgetc (fd);
                      if ( c == '&')
                         estado = 24;
                      else {
                        error (2);
                        estado = 0;
                        break;
                      }
            case 24 : token = 50;
                      apilar (token, -1);
                      estado = 0;
                      c = fgetc(fd);
                      break;
            case 25 : c = fgetc (fd);
                      if ( c == '-') {
                         token = 36;
                         apilar (token, -1);
                         c = fgetc (fd);
                         estado = 0;
                         break;
                      }
                      else if ( c == '=' ){
                                token = 52;
                                apilar (token, -1);
                                c = fgetc (fd);
                                estado = 0;
                                break;
                           }
                           else estado = 26;
            case 26 : if ( ps[pila-1].p_token == 32  ||
                           ps[pila-1].p_token == 27  ||
                           ps[pila-1].p_token == 58     )
                         token = 40;
                      else token = 63;
                      apilar (token, -1);                                   
                      estado = 0;
                      break;
            case 27 : c = fgetc (fd);
                      if ( c == '=' ){
                                token = 53;
                                apilar (token, -1);
                                c = fgetc (fd);
                                estado = 0;
                                break;
                      }
                      else estado = 28;
            case 28 : if (   ps[pila-1].p_token == 32  ||
                           ( ps[pila-1].p_token == 27  && ps[pila-2].p_token != 26 ) ||
                             ps[pila-1].p_token == 58     )
                         token = 38;
                      else token = 28;
                      apilar (token, -1);
                      estado = 0;
                      break;
	    case 29 : tabla_strings[num_strings] = malloc(40);
		      paux = tabla_strings[num_strings];
		      while (  (c = fgetc(fd)) != '"' )
			*paux++ = c;
		      *paux = '\0';
		      num_strings++;
		      c = fgetc(fd);
		      estado = 0;
            default : break;

        }
    }

    /* Meter $ al final de la pila del scaner */
    ps[pila].p_token = 15;
    ps[pila].p_indice = -1;

    fclose(fd);


    pasada2();
}

formar (indice)
int  indice;

{   if ( indice < CAR_MAX )
        identificador[indice] = c;
}

digito ()

{   if ( (c >= '0') && (c <= '9') )
        return (TRUE);
    else return (FALSE);
}

letra ()
{
    if (  ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z'))  )
        return (TRUE);
    else return (FALSE);
}


ver (indice, identificador )
int  indice;
char identificador[];

{   int p, encontrada = FALSE;

        p = hash (indice, identificador);
        while ( !encontrada && p < MAX_TS ) {
            if (ts[p].nombre[0] == '\0') return (-1);
                encontrada = comparar (identificador, ts[p].nombre);
                 p++;
        }
        if ( encontrada )
                return (p-1);
        else {
                p = 0;
                while ( !encontrada && p < MAX_TS ) {
                 if (ts[p].nombre[0] == '\0') return (-1);
                 encontrada = comparar (identificador, ts[p].nombre);
                 p++;
                }
                if ( encontrada )
                 return (p-1);
                else
                 return (-1);
        }
}


insertar (indice, cadena)
int indice;
char cadena[];

{   int donde, ocupado = TRUE;

    donde = hash (indice, cadena);

    if ( ts[donde].nombre[0] == '\0' ) {
            ocupado = FALSE;
            meter_en_ts (donde, indice, cadena);
            return (donde);
    }

    if ( ocupado ) {
        /* tratar la colision */
        while ( ts[donde].nombre[0] != '\0'  &&  donde < MAX_TS )
                donde++ ;
        if ( donde >= MAX_TS ) {
            donde = 0;
            while ( ts[donde].nombre[0] != '\0'  &&  donde < MAX_TS )
                    donde++ ;
            if ( donde >= MAX_TS )
                    printf ("\n\n ERROR: DESBORDAMIENTO EN LA TABLA DE SIMBOLOS \n\n");
            else {
                meter_en_ts (donde, indice, cadena);
                return (donde);
            }
        }
        else {
            meter_en_ts (donde, indice, cadena);
            return (donde);
        }
    }
}

hash (indice, cadena)
int indice;
char cadena[];

{   int aux = 0, i;

    for (i = 0; i < indice ; i++)
        aux = aux + cadena[i];
    return (aux % MAX_TS);
}

meter_en_ts (lugar, indice, cadena)
int lugar, indice;
char cadena[];

{   int i;

    for (i = 0; i < indice ; i++)
        ts[lugar].nombre[i] = cadena[i];

}

apilar (token, punt)
int token, punt;

{
    ps[pila].p_token = token;
    ps[pila].p_indice = punt;
    pila++;
}

reservada (tok, indice)
int *tok, indice;

{   int p;

    p = hash (indice, identificador);

    if ( !ts[p].bloque )
        *tok = ts[p].tipo;
}

comparar (s, t)
char s[], t[];

{   int i = 0;

    while ( s[i] == t[i] )
        if ( s[i++] == '\0' )
            return (TRUE);
    return (FALSE);
}

error (tipo)
int tipo;

{
   if ( tipo == 1 || tipo == 2 ) {
	error_lexico = TRUE;
	printf ("\n\n << ERROR LEXICO >>\n\n");
   }
   else if (tipo == 10 || tipo == 15)
	printf ("\n\n << ERROR DE SINTAXIS >>\n\n");
   else if ( tipo >= 100 && tipo <= 200 ) {
	error_lexico = TRUE;
	if ( tipo == 100 )
		printf ("\n\n << ERROR: DOBLE DECLARACION DE VARIABLES >>\n\n");
	else if ( tipo == 101 )
		printf ("\n\n << ERROR: IDENTIFICADOR NO DECLARADO >>\n\n");
	else if ( tipo == 102 )
		printf ("\n\n << ERROR: LLAMADA A FUNCION NO DECLARADA >>\n\n");
	else if ( tipo == 103 )
		printf ("\n\n << ERROR: FUNCION 'MAIN' NO DECLARADA >>\n\n");
	else if ( tipo == 104 )
		printf ("\n\n << ERROR: DOBLE DECLARACION DE FUNCION >>\n\n");
	else if ( tipo == 105 )
		printf ("\n\n << ERROR: TIPO DE ESTRUCTURA NO DECLARADO >>\n\n");
   }
   else if ( tipo >= 300 )
	printf ("\n\n << ERROR EN EL ACCESO A ESTRUCTURA >>\n\n");


   exit();

}




/* De aqu¡ para abajo est  el parser */

struct  gramatica { char    ante;
                    char    *conse;
        }  g[MAX_REGLAS];

struct  {   char simbolo;                       
            int sem; 
            char relacion;}  pp[MAX_PP];    /* pila del parser */

 

parser ()

{   char    *cadena, *aux;
    int     err = FALSE, k = 0, i = 0;
    char    rel, nuevo=-1;
    char    precedencia(), reduccion();

    inicializar_r();
    inicializar_gr();


    cadena = malloc(20);
    pp[0].simbolo = 15;
    pp[0].sem = -1;

    while ( (nuevo != 70 || ps[i+1].p_token != 15) && !err  && !kbhit() ) {
        *cadena = '\0';
        c = 0;
        aux = cadena;
        while ( (rel = precedencia (pp[k].simbolo, ps[i+1].p_token)) != 'M'  &&  !err )
                    if ( rel == '.')
                            err = TRUE;
                    else {
                        pp[k].relacion = rel;
                        k++;
                        i++;
                        pp[k].simbolo = ps[i].p_token;
                        pp[k].sem = ps[i].p_indice;
                    }
        if ( err )
		error(10);
        else {
            pp[k].relacion = 'M';
            while ( pp[k].relacion != 'm' ) {
                    *aux++ = pp[k].simbolo;
                    c++;
                    k--;
            }
            *aux = '\0';
            invertir (aux, c);
            nuevo = reduccion (cadena, &err, k, i);
            pp[k].relacion = precedencia (pp[k].simbolo, nuevo);
            k++;
            pp[k].simbolo = nuevo;
        }
    }
    if ( nuevo == 70 )
	printf ("\n\n<< COMPILACION FINALIZADA CORRECTAMENTE.\n");
	printf ("\n   EL FICHERO EJECUTABLE ES 'OBJETO'.  >>\n\n");
    free (cadena);
}



invertir (s, c)
char *s;
int  c;

{   char *provisional, *aux;
    int  i;

    provisional = (char *) malloc (20);
    aux = provisional;
    s--;
    for (i = 1; i <= c; i++)
        *provisional++ = *s--;
    s++;
    for (i = 1; i <= c; i++)
        *s++ = *aux++;
    *s = '\0';
}

char reduccion (cadena, err, k, indice_ps)
char *cadena;
int  *err, k, indice_ps;

{   int i, encontrada = FALSE;

    for (i = 0; i <= 107   && !encontrada; i++)
        encontrada = comparar (cadena, g[i].conse);
    if ( encontrada ) {
        intermedio (i-1, k, indice_ps, err);
        return ( g[i-1].ante);
    }
    else {
        error (15);
        *err = TRUE;
    }
}


char precedencia (t1, t2)
char t1, t2;

{   int x, y;

    x = (int) t1;
    y = (int) t2;
    if (x == 70)
        x = 0;
    if (y == 70)
        y = 0;
        
    return (r[x][y]);
 }

    



/* De aqu¡ para abajo est  el generador de c¢digo */
int i = 0;          /* indice de la tabla de codigo intermedio */
int temp = 0;  /* numero de variable temporal */
struct {  int   operador;
          int   op1;
          int   op2;    } ci[MAX_CI];
int es_declaracion = FALSE, es_main = FALSE, indireccion = FALSE;
int numetiq = 0;
int acceso_est = FALSE, dpto = -1;

intermedio (r, k, indice_ps, err)
int r, k, indice_ps;
int *err;

{   int clave, numpar;

    /* comprobar si es una sentecia case.
       generar las etiquetas             */
    if ( (r >= 70 && r<= 93) || r == 100 )
        if ( pp[k].simbolo == 68 ) {
                fprintf (fd2,"__etiq%d%d:",numcomp-1,numetiq);
                numetiq++;
        }

    if  (r >= 14  &&  r <= 32)
        pp[k+1].sem = pp [k+1].simbolo;
    else if ( (r >= 33 && r <= 37) || r == 100 )
            es_declaracion = FALSE;
        else if ( r >= 50 &&  r <= 56 ) {
                numpar = r - 50;
                if ( ps[indice_ps+1].p_token == 37 ) {
		    /* es llamada a funcion */
		    if ( comparar(ts[pp[k+1].sem].nombre,"printf") )
			llamar_pf(numpar-1,k);
		    else {
		       out_parametros (numpar, k);
		       ci[i].operador = 1;
		       ci[i].op1 = pp[k+1].sem;
		       codigo (0);
		       i++;
		       if ( pp[k].simbolo == 13 ){
			    /* es el caso de id opasign llamfunc */
			    if ( pp[k-2].simbolo == 28) {
			      /* es el caso de *id opasign llamfunc */
			      indireccion = TRUE;
			     }
			     ci[i].operador = pp[k].sem;
			     ci[i].op1 = pp[k-1].sem;
			     codigo (0);
			     i++;
			     if (temp > maxi)
			       maxi = temp;
			     temp = 0;
		       }
                    }

                }
                else {
                         /* es declaracion de funcion */
                         ci[i].operador = 2;
			 ci[i].op1 = pp[k+1].sem;
                         codigo (0);
                         in_parametros (numpar, indice_ps);
                         i++;
                     }
                if (temp > maxi)
                    maxi = temp;
                temp = 0;
             }
            else if ( r >= 87  &&  r <= 90 ) {
                    clave = r - 87;
                    ci[i].operador = pp[k+2].sem;
                    ci[i].op1 = pp[k+1].sem;
                    ci[i].op2 = pp[k+3].sem;
                    codigo (clave);
                    pp[k+1].sem = temp;
		    temp++;
		    if (temp > maxi)
		       maxi = temp;
                    i++;
                }
                else if ( r == 81  ||  r == 82 ||  r == 83 ) {
                            clave = r - 81;
                            ci[i].operador = pp[k+1].simbolo;
                            ci[i].op1 = pp[k+2].sem;
                            codigo (clave);
                            pp[k+1].sem = temp;
                            temp++;
                            i++;
                    }
                    else switch (r) {
                            case 4  :
                            case 5  : if ( es_main ) {
                                        fprintf (fd2,"STOP\n");
                                        es_main = FALSE;
                                      }
                                      else fprintf (fd2,"RET\n");
                                      break;
                            case 44 :
                            case 45 : fprintf (fd2, "RET\n");
                                      break;
                            case 48 : ci[i].operador = pp[k+2].sem;
                                      ci[i].op1 = pp[k+1].sem;
                                      codigo (0);
                                      i++;
                                      if (temp > maxi)
                                        maxi = temp;
                                      temp = 0;
                                      break;
			    case 49:  indireccion = TRUE;
				      ci[i].operador = pp[k+3].sem;
				      ci[i].op1 = pp[k+2].sem;
				      codigo (0);
				      i++;
				      if (temp > maxi)
					 maxi = temp;
				      temp = 0;
                                      break;
			    case 58 : dpto = calcular_dpto (pp[k+1].sem, ts[pp[k+3].sem].nombre, err,1);
                                      ci[i].op1 = pp[k+1].sem;
                                      ci[i].operador = 5;
                                      codigo(0);
                                      i++;
                                      pp[k+1].sem = temp;
                                      temp++;
                                      break;
			    case 59 : dpto = calcular_dpto (pp[k+1].sem, ts[pp[k+5].sem].nombre, err,1);
                                      ci[i].op1 = pp[k+1].sem;
                                      ci[i].operador = 5;
                                      codigo(0);
                                      i++;
                                      pp[k+1].sem = temp;
                                      temp++;
                                      break;
                          case 101  : dpto = calcular_dpto (pp[k+2].sem, ts[pp[k+4].sem].nombre, err,-1);
                                      ci[i].op1 = pp[k+2].sem;
                                      ci[i].operador = 5;
                                      codigo(0);
                                      i++;
                                      pp[k+1].sem = temp;
                                      temp++;
                                      break;
                          case 102  : dpto = calcular_dpto (pp[k+2].sem, ts[pp[k+5].sem].nombre, err,-1);
                                      ci[i].op1 = pp[k+2].sem;
                                      ci[i].operador = 5;
                                      codigo(0);
                                      i++;
                                      pp[k+1].sem = temp;
                                      temp++;
                                      break;
                          case 107  : dpto = calcular_dpto (pp[k+1].sem, ts[pp[k+4].sem].nombre, err,1);
                                      ci[i].op1 = pp[k+1].sem;
                                      ci[i].operador = 5;
                                      codigo(0);
                                      i++;
                                      pp[k+1].sem = temp;
                                      temp++;
                                      break;
                            case 60 : fprintf (fd2,"__etiq%d%d:NOP\n",numcomp-1,numetiq);
                                      numetiq++;
                                      prof_sw[prsw] = numetiq;
                                      if ( numcomp > 1 ) {
                                            prsw--;
                                            numetiq = prof_sw[prsw];
                                      }
                                      numcomp--;
				      fprintf (fd2,"__fin%d:",nro_sw);
				      nro_sw++;
                                      break;
                            case 65 :
			    case 66 : fprintf (fd2, "J __fin%d\n",nro_sw);
                                      if ( ps[indice_ps+2].p_token == 69 ) {
                                        /* vemos el caso de que la siguiente sentencia
                                          etiquetada sea un default   */
                                        fprintf (fd2,"__etiq%d%d:",numcomp-1,numetiq);
                                        numetiq++;
                                      }
                                      break;
                            case 68 : fprintf(fd2,"NOP\n");
                                      break;
                            case 70 : /* ver si el identificador esta siendo declarado; si
                                         es as¡, no hacer nada  */
                                      if ( pp[k].simbolo == 4  ||  pp[k].simbolo == 26 ||
                                           pp[k].simbolo == 14  )
                                                es_declaracion = TRUE;
                            case 73 : if ( !es_declaracion ) {
                                          clave = r - 70;
                                          /* reduccion por EXPR -> CD | ID */
                                          ci[i].operador = 0;
                                          ci[i].op1 = pp[k+1].sem;
                                          codigo (clave);
                                          pp[k+1].sem = temp;
                                          i++;
                                          temp++;
                                      }
				      break;
			    case 72 : ci[i].operador = 6;
				      ci[i].op1 = pp[k+1].sem;
				      codigo(0);
				      i++;
				      temp++;
				      break;
                            case 74 : /* ver si el identificador esta siendo declarado; si
                                         es as¡, no hacer nada  */
                                      if ( pp[k].simbolo == 4  ||  pp[k].simbolo == 26 ||
                                           pp[k].simbolo == 14  )
                                                es_declaracion = TRUE;
                                      if ( !es_declaracion ) {
                                          /* reduccion por EXPR -> *ID */
                                          ci[i].operador = 0;
                                          ci[i].op1 = pp[k+2].sem;
                                          codigo (5);
                                          pp[k+1].sem = temp;
                                          i++;
                                          temp++;
                                      }
                                      break;
                            case 75 : fprintf(fd2,"LDA (__T%d)\n",pp[k+3].sem);
                                      fprintf(fd2,"STA __T%d\n",temp);
                                      pp[k+1].sem = temp;
                                      temp++;
                                      break;
                            case 76 : /* regla (EXPR). Si es del
                                        switch tratarlo   */
                                      if ( pp[k].simbolo == 65 ) {
                                          /* es switch */
                                          fprintf (fd2, "STA  __comp%d\n",numcomp);
                                          if ( numcomp > 0 ) {
                                             prof_sw[prsw] = numetiq;
                                             prsw++;
                                             numetiq = prof_sw[prsw];
                                          }
                                          numcomp++;
                                      }
                                      break;
                            case 77 : fprintf(fd2,"LDA %s\n",ts[pp[k+2].sem].nombre);
                                      fprintf(fd2,"STA __T%d\n",temp);
                                      pp[k+1].sem = temp;
				      fprintf(fd2,"NOT \n",temp);
				      fprintf(fd2,"STA __T%d\n",temp);
                                      temp++;
                                      break;
                            case 78 : fprintf(fd2,"LDA __T%d\n",pp[k+3].sem);
                                      pp[k+1].sem = pp[k+3].sem;
				      fprintf(fd2,"NOT \n",pp[k+3].sem);
				      fprintf(fd2,"STA __T%d\n",pp[k+1].sem);
                                      break;
                            case 79 :
                            case 80 : ci[i].operador = pp[k+2].simbolo;
                                      ci[i].op1 = pp[k+1].sem;
                                      clave = r - 79;
                                      codigo(clave);
                                      pp[k+1].sem = temp;
                                      temp++;
                                      i++;
                                      break;
                            case 91 : ci[i].operador = pp[k+4].sem;
                                      ci[i].op1 = pp[k+2].sem;
                                      ci[i].op2 = pp[k+6].sem;
                                      codigo (4);
                                      i++;
                                      pp[k+1].sem = temp;
				      temp++;
				      if (temp > maxi)
					maxi = temp;
                                      break;
                            case 92 :
                            case 93 : indireccion = TRUE;
                                      ci[i].operador = pp[k+3].simbolo;
                                      ci[i].op1 = pp[k+2].sem;
                                      clave = r - 92;
                                      codigo(clave);
                                      pp[k+1].sem = temp;
                                      temp++;
                                      i++;
                                      break;
                            case 97 : acceso_est = TRUE;
                                      dpto = calcular_dpto (pp[k+1].sem, ts[pp[k+3].sem].nombre, err,1);
                                      ci[i].op1 = pp[k+1].sem;
                                      ci[i].operador = pp[k+4].sem;
                                      codigo(0);
                                      i++;
                                      if (temp > maxi)
                                        maxi = temp;
                                      temp = 0;
                                      break;
                            case 98 : acceso_est = TRUE;
				      dpto = calcular_dpto (pp[k+1].sem, ts[pp[k+5].sem].nombre, err,1);
                                      ci[i].op1 = pp[k+1].sem;
                                      ci[i].operador = pp[k+6].sem;
                                      codigo(0);
                                      i++;
                                      if (temp > maxi)
                                        maxi = temp;
                                      temp = 0;
                                      break;
                          case 103  : acceso_est = TRUE;
                                      dpto = calcular_dpto (pp[k+2].sem, ts[pp[k+4].sem].nombre, err,-1);
                                      ci[i].op1 = pp[k+2].sem;
                                      ci[i].operador = pp[k+5].sem;
                                      codigo(0);
                                      i++;
                                      if (temp > maxi)
                                        maxi = temp;
                                      temp = 0;
                                      break;
                          case 104  : acceso_est = TRUE;
                                      dpto = calcular_dpto (pp[k+2].sem, ts[pp[k+5].sem].nombre, err,-1);
                                      ci[i].op1 = pp[k+2].sem;
                                      ci[i].operador = pp[k+6].sem;
                                      codigo(0);
                                      i++;
                                      if (temp > maxi)
                                        maxi = temp;
                                      temp = 0;
                                      break;
                          case 106  : acceso_est = TRUE;
                                      dpto = calcular_dpto (pp[k+1].sem, ts[pp[k+4].sem].nombre, err,1);
                                      ci[i].op1 = pp[k+1].sem;
                                      ci[i].operador = pp[k+5].sem;
                                      codigo(0);
                                      i++;
                                      if (temp > maxi)
                                        maxi = temp;
                                      temp = 0;
				      break;

                        }
    /* comprobar si es sentencia case.
       Generar la comprobacion  de la etiqueta */
    if ( (r >= 70 && r<= 93) || r == 100 )
        if ( pp[k].simbolo == 68 ) {
                fprintf (fd2,"SUB  __comp%d\n",numcomp-1);
                fprintf (fd2,"JNZ  __etiq%d%d\n",numcomp-1,numetiq);
        }


}

in_parametros (numpar, indice_ps)
int numpar, indice_ps;

{   int aux = indice_ps - 1;

    for (i = 0; i < numpar; i++) {
        fprintf (fd2, "POP %s \n", ts[ps[aux].p_indice].nombre);
        aux -= 2;
    }
    if ( !comparar ("main",ts[ci[i].op1].nombre) )
        fprintf (fd2, "PUSH dir_retorno\n");

}

out_parametros (numpar, k)
int numpar, k;

{   int aux = k + 3;

    for (i = 0; i < numpar; i++) {
        fprintf (fd2, "PUSH __T%d\n", pp[aux].sem);
        aux += 2;
    }
}

llamar_pf(np,k)
int np, k;

{ int i, aux = 2*np;

  for (i = 0; i < np; i++) {
	fprintf (fd2, "PUSH __T%d\n",pp[(k+3)+aux].sem);
	aux -= 2;
  }
  fprintf (fd2, "PUSH,i __s%d\n",ns);
  ns++;
  fprintf (fd2, "CALL printf\n");
}


calcular_dpto (ind, cad, err, dif)
char cad[];
int ind, *err;

{   int aux;

    aux = ts[ind].tipo*dif - 30;
    if ( ts[ind].tipo*dif < 30 ) {
            *err = TRUE;
            error (301);
    }
    else {
	    while ( tabla_est[aux].desplazamiento != -1 ) {
                if ( comparar(tabla_est[aux].nombre, cad) )
                        return(tabla_est[aux].desplazamiento);
		else aux++;
            }
	    /* si se llega aqui error */
            *err = TRUE;
            error (300);
    }
}


int sanhachis = 0, nlute = 0;
/* llevan la cuenta de variables para comparaciones */

codigo (clave)
int clave;

{   char *tipo_oper;

    if ( (ci[i].operador >= 38  &&  ci[i].operador <= 41) ||
	 (ci[i].operador >= 48  &&  ci[i].operador <= 50) ||
	 (ci[i].operador >= 42  &&  ci[i].operador <= 47)    ) {
        switch (ci[i].operador) {
            case 38 : tipo_oper = "MUL";break;
	    case 39 : tipo_oper = "ADD";break;
	    case 42 :
	    case 41 : tipo_oper = "DIV";break;
	    case 49 : tipo_oper = "OR";break;
	    case 50 : tipo_oper = "AND";break;
	    default : tipo_oper = "SUB";break;

        }
        if ( clave == 0 ) {
            /* por ejemplo: 8 * 3 */
            fprintf (fd2, "LDA,i  %d\n", ci[i].op1);
            fprintf (fd2, "%s,i  %d\n", tipo_oper,ci[i].op2);
            fprintf (fd2, "STA  __T%d\n", temp);
        }
        else if ( clave == 1 ) {
              /* por ejemplo: 8 * a */
              fprintf (fd2, "LDA,i  %d\n", ci[i].op1);
              fprintf (fd2, "%s  %s\n", tipo_oper, ts[ ci[i].op2 ].nombre);
              fprintf (fd2, "STA  __T%d\n", temp);
            }
            else if ( clave == 2 ) {
                    /* por ejemplo: a * 8 */
                    fprintf (fd2, "LDA  %s\n", ts[ ci[i].op1 ].nombre );
                    fprintf (fd2, "%s,i  %d\n", tipo_oper,ci[i].op2);
                    fprintf (fd2, "STA  __T%d\n", temp);
                }
                else if ( clave == 3 ) {
                        /* por ejemplo: a * b */
                        fprintf (fd2, "LDA   %s\n", ts[ ci[i].op1 ].nombre );
                        fprintf (fd2, "%s  %s\n", tipo_oper, ts[ ci[i].op2 ].nombre);
                        fprintf (fd2, "STA  __T%d\n", temp);
                    }
                    else if ( clave == 4 ) {
                            /* es el caso de (EXPR) op (EXPR) */
                            fprintf (fd2, "LDA  __T%d\n",ci[i].op1 );
                            fprintf (fd2, "%s  __T%d\n", tipo_oper, ci[i].op2 );
                            fprintf (fd2, "STA  __T%d\n", temp);
			}
	switch (ci[i].operador) {
		/* tratar ==  <  >  >=  <=  */

		case 48 : fprintf (fd2, "NOT\n");break;
		case 43 : fprintf (fd2, "JM __ayl%d\n",sanhachis);
			  fprintf (fd2, "J __lya%d\n",sanhachis);
			  fprintf (fd2, "__ayl%d : SUB __T%d\n",sanhachis,temp);
			  fprintf (fd2, "__lya%d : NOP\n",sanhachis);
			  sanhachis++;
			  break;
		case 44 : fprintf (fd2, "JP __ayl%d\n",sanhachis);
			  fprintf (fd2, "J __lya%d\n",sanhachis);
			  fprintf (fd2, "__ayl%d : SUB __T%d\n",sanhachis,temp);
			  fprintf (fd2, "__lya%d : NOP\n",sanhachis);
			  sanhachis++;
			  break;
		case 45 : fprintf (fd2, "JM __ayl%d\n",sanhachis);
			  fprintf (fd2, "JZ __lute%d\n",nlute);
			  fprintf (fd2, "J __lya%d\n",sanhachis);
			  fprintf (fd2, "__ayl%d : SUB __T%d\n",sanhachis,temp);
			  fprintf (fd2, "J __lya%d\n",sanhachis);
			  fprintf (fd2, "__lute%d : NOT\n",nlute);
			  fprintf (fd2, "__lya%d : NOP\n",sanhachis);
			  sanhachis++;
			  nlute++;
			  break;
		case 46 : fprintf (fd2, "JP __ayl%d\n",sanhachis);
			  fprintf (fd2, "JZ __lute%d\n",nlute);
			  fprintf (fd2, "J __lya%d\n",sanhachis);
			  fprintf (fd2, "__ayl%d : SUB __T%d\n",sanhachis,temp);
			  fprintf (fd2, "J __lya%d\n",sanhachis);
			  fprintf (fd2, "__lute%d : NOT\n",nlute);
			  fprintf (fd2, "__lya%d : NOP\n",sanhachis);
			  sanhachis++;
			  nlute++;
			  break;
		/* tratar modul% */
		case 42 : fprintf (fd2, "STR __T%d\n",temp);
			  fprintf (fd2, "LDA __T%d\n",temp);
			  break;
	}
    }
    else switch (ci[i].operador) {
            case 63 :   /* menos unoario */
                        if ( clave == 0 )
                            /* es el caso de  -ID */
                            fprintf (fd2, "LDA  %s\n", ts[ ci[i].op1 ].nombre);

                        else if ( clave == 1 )
                                /* es el caso de -5 */
                                fprintf (fd2, "LDA,i  %d\n", ci[i].op1);

                              else  /* es el caso de  -(EXPR)  */
                                    fprintf (fd2, "LDA  __T%d\n", temp-1);
                        fprintf (fd2, "MUL,i -1\n");
                        fprintf (fd2, "STA  __T%d\n",temp);
                        break;

            case 35 :
            case 36 : if ( !indireccion )
                            fprintf (fd2, "LDA  %s\n", ts[ ci[i].op1 ].nombre);
                      else {
                            fprintf (fd2, "LDA  (%s)\n", ts[ ci[i].op1 ].nombre);
                            indireccion = FALSE;
                      }
                      fprintf (fd2, "STA  __T%d\n",temp);
                      if ( clave == 0 )
                            fprintf (fd2, "INC  %s\n", ts[ ci[i].op1 ].nombre);
                      else
                            fprintf (fd2, "DEC  %s\n", ts[ ci[i].op1 ].nombre);
                      break;

            case 31 : if ( acceso_est ) {
                        fprintf(fd2, "LDIX,i %s\n",ts[ci[i].op1].nombre);
                        fprintf(fd2, "STA [%d]\n",dpto);
                        acceso_est = FALSE;
                      }
		      else {
                          if ( !indireccion )
                                fprintf (fd2, "STA  %s\n",ts[ ci[i].op1 ].nombre);
                          else {
                                fprintf (fd2, "STA  (%s)\n",ts[ ci[i].op1 ].nombre);
                                indireccion = FALSE;
                          }
                      }
                      break;
            case 51 : fprintf (fd2, "ADD  %s\n",ts[ ci[i].op1 ].nombre);
                      if ( !indireccion )
                            fprintf (fd2, "STA  %s\n",ts[ ci[i].op1 ].nombre);
                      else {
                            fprintf (fd2, "STA  (%s)\n",ts[ ci[i].op1 ].nombre);
                            indireccion = FALSE;
                      }
                      break;
	    case 52 : fprintf (fd2, "SUB  %s\n",ts[ ci[i].op1 ].nombre);
		      fprintf (fd2, "MUL,i -1\n");
                      if ( !indireccion )
                            fprintf (fd2, "STA  %s\n",ts[ ci[i].op1 ].nombre);
                      else {
                            fprintf (fd2, "STA  (%s)\n",ts[ ci[i].op1 ].nombre);
                            indireccion = FALSE;
                      }
                      break;

            case 53 : fprintf (fd2, "MUL  %s\n",ts[ ci[i].op1 ].nombre);
                      if ( !indireccion )
                            fprintf (fd2, "STA  %s\n",ts[ ci[i].op1 ].nombre);
                      else {
                            fprintf (fd2, "STA  (%s)\n",ts[ ci[i].op1 ].nombre);
                            indireccion = FALSE;
                      }
                      break;                      

            case 54 : fprintf (fd2, "DIV  %s\n",ts[ ci[i].op1 ].nombre);
                      if ( !indireccion )
                            fprintf (fd2, "STA  %s\n",ts[ ci[i].op1 ].nombre);
                      else {
                            fprintf (fd2, "STA  (%s)\n",ts[ ci[i].op1 ].nombre);
                            indireccion = FALSE;
                      }
                      break;

            case 0  : /* reduccion por EXPR -> cd | id | *id      */
                      if ( clave == 5 )
                        fprintf (fd2, "LDA  (%s)\n", ts[ ci[i].op1 ].nombre);
                      else {
                          if ( clave == 0 )
                            fprintf (fd2, "LDA  %s\n", ts[ ci[i].op1 ].nombre);
                          else  fprintf (fd2, "LDA,i  %d\n", ci[i].op1);
                      }
                      fprintf (fd2, "STA  __T%d\n",temp);
                      break;
            case 1  : /* es llamada a funcion */
                      fprintf (fd2, "CALL %s\n", ts[ci[i].op1].nombre);
                      break;
            case 2  : /* es declaracion de funcion */
                      if ( !comparar ("main",ts[ci[i].op1].nombre) )
                        fprintf (fd2, "%s: POP dir_retorno\n",ts[ci[i].op1].nombre);
                      else {
                        fprintf (fd2, "main:");
                        es_main = TRUE;
                      }
                      break;
            case 5  : /* VALOR1 ->ID.ID  y parecidas */
                      fprintf(fd2, "LDIX,i %s\n",ts[ci[i].op1].nombre);
                      fprintf(fd2, "LDA [%d]\n",dpto);
                      fprintf(fd2, "STA __T%d\n",temp);
                      break;
	    case 6  : /* EXPR -> CC  */
		      fprintf(fd2, "LDA,i %d\n",ci[i].op1);
		      fprintf(fd2, "STA __T%d\n",temp);
		      break;
    }

}

hacer_printf()
{
	fprintf (fd2, "\n\n\n");
	fprintf (fd2, "printf : POP dir_retorno\n");
	fprintf (fd2, "POP __pfs\n");
	fprintf (fd2, "__buc : LDA (__pfs)\n");
	fprintf (fd2, "JZ __finpf\n");
	fprintf (fd2, "SUB,i '%'\n");
	fprintf (fd2, "JZ __porcentaje\n");
	fprintf (fd2, "LDA (__pfs)\n");
	fprintf (fd2, "SUB,i '\\\\'\n");
	fprintf (fd2, "JZ __slash\n");
	fprintf (fd2, "WRITE (__pfs)\n");
	fprintf (fd2, "INC __pfs\n");
	fprintf (fd2, "J __buc\n");
	fprintf (fd2, "__porcentaje : INC __pfs\n");
	fprintf (fd2, "LDA (__pfs)\n");
	fprintf (fd2, "SUB,i 'd'\n");
	fprintf (fd2, "JZ __int\n");
	fprintf (fd2, "POP __argpf\n");
	fprintf (fd2, "WRITE __argpf\n");
	fprintf (fd2, "INC __pfs\n");
	fprintf (fd2, "J __buc\n");
	fprintf (fd2, "__int : POP __argpf\n");
	fprintf (fd2, "WRINT __argpf\n");
	fprintf (fd2, "INC __pfs\n");
	fprintf (fd2, "J __buc\n");
	fprintf (fd2, "__slash : INC __pfs\n");
	fprintf (fd2, "WRITE,i '\\n'\n");
	fprintf (fd2, "INC __pfs\n");
	fprintf (fd2, "J __buc\n");
	fprintf (fd2, "__finpf : PUSH dir_retorno\n");
	fprintf (fd2, "RET\n");
}

