#define	    FALSE 0
#define     TRUE 1
#define     CAR_MAX   15
#define     MAX_TS   50
#define     MAX_PS  200
#define     MAX_ESTRUC  30
#define     MAX_FUNC  30
#define     MAX_PARAMETROS  10

extern  struct  {   int p_token;
                    int p_indice; }  ps[MAX_PS];    /* pila del escaner */

extern  struct {    char nombre[CAR_MAX];
                    int tipo;
                    int bloque;             } ts[MAX_TS];   /* tabla de simbolos */

extern struct { char nombre[CAR_MAX];
                int desplazamiento;
                int tipo;
                int profundidad;    }tabla_est[MAX_ESTRUC]; /*tabla de simbolos de estructuras*/


int  err, posicion = 0;

pasada2()
{ int   i = 1, est = 0, bq_actual = 1, hay_main = FALSE, apuntador = FALSE, escoma = FALSE;
  int   tip, j, k = 0, aux, profundidad = 1, cont = 0,  tipoestruct = 30;
  int   declaradas[MAX_FUNC];
  char  nuevo[CAR_MAX];
  int   parametros[MAX_PARAMETROS];
  int   p = 0;
  
  err = FALSE;
  while ( ps[i].p_token != 15  &&  !err && !kbhit())
    switch (est) {
        case 0 : if ( ps[i].p_token == 29 ) {
                   tip = 2;
                   est = 1;
                 }
                 else if ( ps[i].p_token == 30 ) {
                         tip = 3;
                         est = 1;
                     }
                     else if ( ps[i].p_token == 27 ) 
                                est = 2;
                          else if ( ps[i].p_token == 26 ) {
                                    profundidad = 1;
                                    cont = 0;
                                    est = 4;
                               } 
                 i++;
                 break;
                
        case 1 : if ( ps[i].p_token == 28 ) {
                        /* es de tipo apuntador */
                        if ( tip >= 30) 
                          tip = -tip; /* apuntador a estructura */
                        else tip = 20; 
                        i++;
                        break;
                 }
                 if ( ps[i].p_token == 27 ) {
                    if ( tip >= 30 || tip <= -30 )
                        tipoestruct = 30 + posicion;
                    if ( bq_actual == 1) {
                        /* vemos la declaracion de var. globales */
                        if ( ts[ps[i].p_indice].tipo == -1 ) {
                            ts[ps[i].p_indice].tipo = tip;
                            ts[ps[i].p_indice].bloque = 1;
                        }
                        else  {
                                /* doble declaracion de dos var. global */
                                err = TRUE;
                                error (100);
                         }
                    }
                    else meter_nuevo (bq_actual, tip, i);
                 }                                                          
                 est = 0;
                 i++;
                 if ( ps[i].p_token == 56 ) {
                    est = 1;
                    i++;
                 }
                 break;
                 
        case 2 : if ( ps[i].p_token != 57 ) {
                    /* pasamos -1 para ver si el id. esta declarado */
                    meter_nuevo (bq_actual, -1, i-1);
                    est = 0;
                 }
                 else 
                    est = 3;
                 i++;
                 break;
                 
        case 3 : j = 0;
                 while ( ts[ps[i-2].p_indice].nombre[j] != '\0' ) {
                        nuevo[j] = ts[ps[i-2].p_indice].nombre[j];
                        j++;
                 }
                 nuevo[j] = '\0';
                 aux = ps[i-2].p_indice;
                 while ( ps[i].p_token != 29 && ps[i].p_token != 30 && ps[i].p_token != 26 &&
                         ps[i].p_token != 59 && ps[i].p_token != 37 && ps[i].p_token != 15    ){
                                if ( ps[i].p_token == 27 ) {
                                        parametros[p] = i;
                                        p++;
                                }
                                i++;
                 }
                 if ( ps[i].p_token == 37 ) {
                    int in;
                    for (in = 0; in < p && !kbhit(); in++)
                        meter_nuevo(bq_actual, -1, parametros[in]);
                    p = 0;
                    declaradas[k] = aux;
                    k++;
                 }
                 else {
                    ts[aux].tipo = 0;
                    /* si es una declaracion de funcion
                       vemos si ya ha sido declarada; si
                       es asi, error: doble declaracion  */
                    if ( ts[aux].bloque > 0 ) {
                        err = TRUE;
                        error (104);
                    }
                    else {  int in;
                            ts[aux].bloque = 1;
                            bq_actual++;
                            for (in = 0; in < p && !kbhit(); in++)
                                meter_nuevo(bq_actual, 8, parametros[in]);
                            p = 0;
                            if ( !hay_main )
                                hay_main = comparar (nuevo, "main");
                            i--;
                    }
                   }
                 i++;
                 est = 0;
                 break;

        case 4 : if ( ps[i].p_token == 29 || ps[i].p_token == 30 || escoma) {
                    if ( escoma )
                         escoma = FALSE;
                    i++;
                    if ( ps[i].p_token == 28 ) {
                        i++;
                        apuntador = TRUE;
                    }
                    if ( ps[i].p_token == 27 ) {
                        if ( !apuntador ) {
                            if ( ps[i-1].p_token == 29 )
                                tip = 2;
                            else tip = 3;
                        }
                        else {
                            tip = 20;
                            apuntador = FALSE;
                        }
                        meter_estruct (ts[ps[i].p_indice].nombre, tip, cont, profundidad);
                        ps[i].p_indice = posicion;
                        cont++;
                        i++;
                        posicion++;
                        est = 4;
                    }
                    if ( ps[i].p_token == 56 ) {
                        est = 4;
                        escoma = TRUE;
                    }

                 }
                 else if ( ps[i].p_token == 26 ) {
                        profundidad++;
                        i++;
                        est = 4;
                      }
                      else if ( ps[i].p_token == 60 ) {
                                if ( profundidad == 1 ) {
                                    tip = tipoestruct;
                                    i++;
                                    posicion++;
                                    est = 1;
                                }
                                else {
                                    i++;
                                    if (  ps[i].p_token == 28 ) {
                                        apuntador = TRUE;
                                        i++;
                                    }
                                    if (  ps[i].p_token == 27 ) {
                                            if (apuntador) {
                                                tip = 21;
                                                apuntador = FALSE;
                                            }
                                            else tip = 4;
                                            meter_estruct (ts[ps[i].p_indice].nombre, tip, 4, profundidad);
                                            ps[i].p_indice = posicion;
                                            posicion++;
                                            profundidad--;
                                    }
                                    i++;
                                    est = 4;
                                }
                           }
                           else if (  ps[i].p_token == 27 ) {
                                    /* tipo de estructura */
                                    if ( ps[i+1].p_token == 59 ) {
                                        ts[ ps[i].p_indice ].tipo = tipoestruct + 70;
                                        ts[ ps[i].p_indice ].bloque = bq_actual;
                                        i++;
                                    }
                                    else if ( ts[ ps[i].p_indice ].tipo < 100 ) {
                                            err = TRUE;
                                            error (105);
                                        }
                                        else {
                                          /* identificador del tipo de estructura */
                                            i++;
                                            tip = ts[ ps[i-1].p_indice ].tipo - 70;
                                            est = 1;
                                        }
                                }
                                else i++;
                 break;
                 
    }
    if (!err) { 
      /* ver posibles llamadas a funciones no declaradas */
      for (j = 0; j < k && !err; j++) 
        if ( ts[declaradas[j]].tipo != 0 ) {
            error (102);
            err = TRUE;
        }
    }
    if (!hay_main && !err)
        error (103);
}

meter_nuevo (b, tip, i)
int b, tip, i;

{  int j = 0, d;
   char n[CAR_MAX];
   char n2[CAR_MAX];

   while ( ts[ps[i].p_indice].nombre[j] != '\0' ) {
            n2[j] = ts[ps[i].p_indice].nombre[j];
            n[j] = ts[ps[i].p_indice].nombre[j];
            j++;
   }
   n2[j] = '\0';
   n[j] = '_';
   j++;
   n[j] = '_';
   j++;
   n[j] = b + '0';
   j++;
   n[j] = '\0';

   if ( tip < 0 ) {
     /* si es componente de struct no hacer nada */
     if ( ps[i-1].p_token != 64 && ps[i-2].p_token != 64) {
             /* solo vemos que indice de la TS le corresponde
               pero no insertamos.Si no esta en su indice es que
	       no ha sido declarado         */
               if ( ( ps[i].p_indice = ver (j,n) ) < 0 ) {
		    /* no esta como local; ver si esta como global */

		    { int i = 0;
		      while (n[i] != '_') {
			n2[i] = n[i];
			i++;
		      }
		    }

                    ps[i].p_indice = ver (j-3,n2);
		    if ( ts[ps[i].p_indice].bloque != 1 ) {
			err = TRUE;
			error (101);
                    }
                }
      }
   }
   else {
       if ( (d = ver (j,n)) >= 0 ) {
            if (ts[d].tipo != 8 ) {
                   /* doble declaracion de dos var. dentro
                      de la misma funci¢n    */
                    err = TRUE;
                    error (100);
            }
        }
	else {
           ps[i].p_indice = insertar (j, n);
           ts[ps[i].p_indice].tipo = tip;
           ts[ps[i].p_indice].bloque = b;
       }
   }

}


meter_estruct (n, tip, cont, p)
char n[];
int tip, cont;

{   int j = 0; 

    while ( n[j] != '\0' ) {
        tabla_est[posicion].nombre[j] = n[j];
        j++;
    }
    tabla_est[posicion].desplazamiento = cont;
    tabla_est[posicion].tipo = tip;
    tabla_est[posicion].profundidad = p;

}

