/******************************************
#     wig compiler-A by:
#
#    David Thibodeau
#    Ioannis Fytilis
#    Sherry Shanshan Ruan
#  
#    copyright @ group-A 2013
#
 ******************************************/

/* Include library functions */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "wigA_run.c"

/* Booleans are represented as ints */

#define true 1
#define false 0

typedef int bool;

/* Define global variables */

char * url;
char * sessionid;
char gb_name[] = "tiny.gb";

FILE *gf;
FILE *lf;

/* Wig global variable section */

int amount;

/* Wig html section */

void html_Welcome(char *url, char *sessionid)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    Welcome!\n  ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

void html_Pledge(char *url, char *sessionid)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    How much do you want to contribute?\n    ");
	printf("<input name=\"contribution\" type=\"text\" size=4>");
	printf("\n  ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

void html_Total(char *url, char *sessionid, char *total)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    The total is now ");
	printf("%s", total);
	printf(".\n  ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

/* Wig session section */

void session_Contribute(int stage)
{
	int i;

	/* Control flow for different stages */
	int step = 0;
	if (stage == 0) goto step_0;

	/* Get the stage where we branch from the file */
	lf = fopen(sessionid, "r");
	step = fgetc(lf);
	fclose(lf);
	if (step == 1) goto step_1;
	if (step == 2) goto step_2;
	exit(1);

/* step_0 : first time into this session */
step_0:

	/* Create a new sessionid for the connection to store locals and globals */
	sessionid = randomString("Contribute", 20);

	/* Read all global variables first */
	gf = fopen(gb_name, "r");
	if (gf != NULL)
	{
		fread(&amount, sizeof(int), 1, gf);
		fclose(gf);
	}

	i = 87;
	
	/* Write local variables into temp file */
	lf = fopen(sessionid, "w");
	fputc(1, lf);
	fwrite(&i, sizeof(int), 1, lf);
	fclose(lf);

	/* Write global variables into temp file */
	gf = fopen(gb_name, "w");
	fwrite(&amount, sizeof(int), 1, gf);
	fclose(gf);

	/* Call to display the html */
	html_Welcome(url, sessionid);
	exit(0);

step_1:

	/* Read local variables from temp file */
	lf = fopen(sessionid, "r");
	fgetc(lf);
	fread(&i, sizeof(int), 1, lf);
	fclose(lf);

	/* Read global variables from temp file */
	gf = fopen(gb_name, "r");
	fread(&amount, sizeof(int), 1, gf);
	fclose(gf);

	
	/* Write local variables into temp file */
	lf = fopen(sessionid, "w");
	fputc(2, lf);
	fwrite(&i, sizeof(int), 1, lf);
	fclose(lf);

	/* Write global variables into temp file */
	gf = fopen(gb_name, "w");
	fwrite(&amount, sizeof(int), 1, gf);
	fclose(gf);

	/* Call to display the html */
	html_Pledge(url, sessionid);
	exit(0);

step_2:

	/* Read local variables from temp file */
	lf = fopen(sessionid, "r");
	fgetc(lf);
	fread(&i, sizeof(int), 1, lf);
	fclose(lf);

	/* Read global variables from temp file */
	gf = fopen(gb_name, "r");
	fread(&amount, sizeof(int), 1, gf);
	fclose(gf);

	i = atoi(getField("contribution"));
	amount = amount+i;
	
	/* Write local variables into temp file */
	lf = fopen(sessionid, "w");
	fputc(2, lf);
	fwrite(&i, sizeof(int), 1, lf);
	fclose(lf);

	/* Write global variables into temp file */
	gf = fopen(gb_name, "w");
	fwrite(&amount, sizeof(int), 1, gf);
	fclose(gf);

	/* We remove session id before exiting */
	remove(sessionid);
	sessionid = "";

	/* Call to display the html */
	html_Total(url, sessionid, itoa(amount));
	exit(0);

}

/* Main function section */

int main (int argc, char *agrv[])
{
	/* Initialize random seed */
	srand48(time((time_t *)0));

	/* Parse fields */
	parseFields();
	url = catString("http://", catString(getenv("SERVER_NAME"), getenv("SCRIPT_NAME")));
	sessionid = getenv("QUERY_STRING");

	/* Decide which service to launch */
	if (sessionid == NULL) goto error_and_exit;

	if (strcmp(sessionid,"Contribute") == 0) session_Contribute(0);
	if (strncmp(sessionid,"Contribute$",11) == 0) session_Contribute(1);

/* Display the error page if an invalid sessionid is passed */
error_and_exit:
	printf("Content-type: text/html\n\n");
	printf("<html><body>\n");
	printf("<h3>Sorry, your session has expired or is not valid.</h3>\n");
	printf("<h3>Valid session(s) are:</h3>\n");
	printf("   --> Contribute<br>");
	printf("<br>Example: <b><i>%s?Contribute</i></b>\n", getenv("SCRIPT_NAME"));
	printf("</body></html>\n");
	exit(1);
}
