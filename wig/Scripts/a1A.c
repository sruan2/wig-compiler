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
char gb_name[] = "a1.gb";

FILE *gf;
FILE *lf;

/* Wig global variable section */

char *msg0 = "", *msg1 = "", *msg2 = "", *msg3 = "";
int connections;

/* Wig html section */

void html_Login(char *url, char *sessionid)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    ");
	printf("<h1>\n");
	printf("Welcome to MyBookmakrs Area");
	printf("</h1>\n");
	printf("\n    ");
	printf("<p>\n");
	printf("\n    Enter your user name:\n    ");
	printf("<input name=\"username\" type=\"text\" size=25>");
	printf("\n    ");
	printf("<p>\n");
	printf("\n    Enter your password:\n    ");
	printf("<input name=\"password\" type=\"text\" size=25>");
	printf("\n  ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

void html_incorrect(char *url, char *sessionid, char *username)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    ");
	printf("<h1>\n");
	printf("I'm sorry ");
	printf("%s", username);
	printf(", permission denied! ");
	printf("</h1>\n");
	printf("\n\n  ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

void html_Update(char *url, char *sessionid, char *msg0l, char *msg1l, char *msg2l, char *msg2, char *msg1, char *msg0)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    ");
	printf("<h1>\n");
	printf("BookMark Service");
	printf("</h1>\n");
	printf("\n    ");
	printf("<hr>\n");
	printf("\n    ");
	printf("<b>\n");
	printf("BookMarks so far:");
	printf("</b>\n");
	printf("<BR>\n");
	printf("\n    ");
	printf("%s", msg0l);
	printf("%s", msg0);
	printf("</a>\n");
	printf("<BR>\n");
	printf("\n    ");
	printf("%s", msg1l);
	printf("%s", msg1);
	printf("</A>\n");
	printf("<BR>\n");
	printf("\n    ");
	printf("%s", msg2l);
	printf("%s", msg2);
	printf("</A>\n");
	printf("<BR>\n");
	printf("\n    ");
	printf("<hr>\n");
	printf("\n    ");
	printf("<b>\n");
	printf("Add bookmark:");
	printf("</b>\n");
	printf("\n    ");
	printf("<p>\n");
	printf("\n    ");
	printf("<input name=\"msg\" type=\"text\" size=70>");
	printf("\n    ");
	printf("<p>\n");
	printf("\n    ");
	printf("<hr>\n");
	printf("\n    ");
	printf("<p>\n");
	printf("\n    ");
	printf("<input name=\"quit\" type=\"radio\" value=\"yes\">");
	printf(" Quit now\n  ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

void html_ByeBye(char *url, char *sessionid, char *msgs, char *conns, char *username)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    ");
	printf("<h1>\n");
	printf("Thanks for visiting, ");
	printf("%s", username);
	printf(". ");
	printf("</h1>\n");
	printf("\n    There have been ");
	printf("%s", conns);
	printf(" connections so far.\n     ");
	printf("<p>\n");
	printf("\n    You added ");
	printf("%s", msgs);
	printf(" bookmarks this time.\n  ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

/* Wig session section */

void session_Addr(int stage)
{
	char *username = "", *password = "", *msg = "", *quit = "";
	int written;
	bool correct;

	/* Control flow for different stages */
	int step = 0;
	if (stage == 0) goto step_0;

	/* Get the stage where we branch from the file */
	lf = fopen(sessionid, "r");
	step = fgetc(lf);
	fclose(lf);
	if (step == 1) goto step_1;
	if (step == 2) goto step_2;
	if (step == 3) goto step_3;
	exit(1);

/* step_0 : first time into this session */
step_0:

	/* Create a new sessionid for the connection to store locals and globals */
	sessionid = randomString("Addr", 20);

	/* Read all global variables first */
	gf = fopen(gb_name, "r");
	if (gf != NULL)
	{
		fread(&connections, sizeof(int), 1, gf);
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			msg0 = (char *)malloc(tmpi+1) ;
			fread(msg0, sizeof(char), tmpi, gf);
			msg0[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			msg1 = (char *)malloc(tmpi+1) ;
			fread(msg1, sizeof(char), tmpi, gf);
			msg1[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			msg2 = (char *)malloc(tmpi+1) ;
			fread(msg2, sizeof(char), tmpi, gf);
			msg2[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			msg3 = (char *)malloc(tmpi+1) ;
			fread(msg3, sizeof(char), tmpi, gf);
			msg3[tmpi] = '\0';
		}
		fclose(gf);
	}

	connections = connections+1;
	correct = false;
	written = 0;

	while(!correct)
	{
		
		/* Write local variables into temp file */
		lf = fopen(sessionid, "w");
		fputc(1, lf);
		{
			int tmpi;
			tmpi = strlen(username);
			fwrite(&tmpi, sizeof(int), 1, lf);
			fwrite(username, sizeof(char), strlen(username), lf);
		}
		fwrite(&correct, sizeof(int), 1, lf);
		{
			int tmpi;
			tmpi = strlen(password);
			fwrite(&tmpi, sizeof(int), 1, lf);
			fwrite(password, sizeof(char), strlen(password), lf);
		}
		{
			int tmpi;
			tmpi = strlen(quit);
			fwrite(&tmpi, sizeof(int), 1, lf);
			fwrite(quit, sizeof(char), strlen(quit), lf);
		}
		{
			int tmpi;
			tmpi = strlen(msg);
			fwrite(&tmpi, sizeof(int), 1, lf);
			fwrite(msg, sizeof(char), strlen(msg), lf);
		}
		fwrite(&written, sizeof(int), 1, lf);
		fclose(lf);

		/* Write global variables into temp file */
		gf = fopen(gb_name, "w");
		fwrite(&connections, sizeof(int), 1, gf);
		{
			int tmpi;
			tmpi = strlen(msg0);
			fwrite(&tmpi, sizeof(int), 1, gf);
			fwrite(msg0, sizeof(char), tmpi, gf);
		}
		{
			int tmpi;
			tmpi = strlen(msg1);
			fwrite(&tmpi, sizeof(int), 1, gf);
			fwrite(msg1, sizeof(char), tmpi, gf);
		}
		{
			int tmpi;
			tmpi = strlen(msg2);
			fwrite(&tmpi, sizeof(int), 1, gf);
			fwrite(msg2, sizeof(char), tmpi, gf);
		}
		{
			int tmpi;
			tmpi = strlen(msg3);
			fwrite(&tmpi, sizeof(int), 1, gf);
			fwrite(msg3, sizeof(char), tmpi, gf);
		}
		fclose(gf);

		/* Call to display the html */
		html_Login(url, sessionid);
		exit(0);

step_1:

		/* Read local variables from temp file */
		lf = fopen(sessionid, "r");
		fgetc(lf);
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, lf);
			username = (char *)malloc(tmpi+1);
			fread(username, sizeof(char), tmpi, lf);
			username[tmpi] = '\0';
		}
		fread(&correct, sizeof(int), 1, lf);
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, lf);
			password = (char *)malloc(tmpi+1);
			fread(password, sizeof(char), tmpi, lf);
			password[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, lf);
			quit = (char *)malloc(tmpi+1);
			fread(quit, sizeof(char), tmpi, lf);
			quit[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, lf);
			msg = (char *)malloc(tmpi+1);
			fread(msg, sizeof(char), tmpi, lf);
			msg[tmpi] = '\0';
		}
		fread(&written, sizeof(int), 1, lf);
		fclose(lf);

		/* Read global variables from temp file */
		gf = fopen(gb_name, "r");
		fread(&connections, sizeof(int), 1, gf);
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			msg0 = (char *)malloc(tmpi+1) ;
			fread(msg0, sizeof(char), tmpi, gf);
			msg0[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			msg1 = (char *)malloc(tmpi+1) ;
			fread(msg1, sizeof(char), tmpi, gf);
			msg1[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			msg2 = (char *)malloc(tmpi+1) ;
			fread(msg2, sizeof(char), tmpi, gf);
			msg2[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			msg3 = (char *)malloc(tmpi+1) ;
			fread(msg3, sizeof(char), tmpi, gf);
			msg3[tmpi] = '\0';
		}
		fclose(gf);

		copyString(&username, getField("username"));
		copyString(&password, getField("password"));

		if((((((strcmp(username,"qxu2")==0)) && ((strcmp(password,"hello")==0)))) || ((((strcmp(username,"group1")==0)) && ((strcmp(password,"group1")==0))))) || ((((strcmp(username,"guest")==0)) && ((strcmp(password,"guest")==0)))))
		{
			correct = true;
		}

		if(!correct)
		{
			
			/* Write local variables into temp file */
			lf = fopen(sessionid, "w");
			fputc(2, lf);
			{
				int tmpi;
				tmpi = strlen(username);
				fwrite(&tmpi, sizeof(int), 1, lf);
				fwrite(username, sizeof(char), strlen(username), lf);
			}
			fwrite(&correct, sizeof(int), 1, lf);
			{
				int tmpi;
				tmpi = strlen(password);
				fwrite(&tmpi, sizeof(int), 1, lf);
				fwrite(password, sizeof(char), strlen(password), lf);
			}
			{
				int tmpi;
				tmpi = strlen(quit);
				fwrite(&tmpi, sizeof(int), 1, lf);
				fwrite(quit, sizeof(char), strlen(quit), lf);
			}
			{
				int tmpi;
				tmpi = strlen(msg);
				fwrite(&tmpi, sizeof(int), 1, lf);
				fwrite(msg, sizeof(char), strlen(msg), lf);
			}
			fwrite(&written, sizeof(int), 1, lf);
			fclose(lf);

			/* Write global variables into temp file */
			gf = fopen(gb_name, "w");
			fwrite(&connections, sizeof(int), 1, gf);
			{
				int tmpi;
				tmpi = strlen(msg0);
				fwrite(&tmpi, sizeof(int), 1, gf);
				fwrite(msg0, sizeof(char), tmpi, gf);
			}
			{
				int tmpi;
				tmpi = strlen(msg1);
				fwrite(&tmpi, sizeof(int), 1, gf);
				fwrite(msg1, sizeof(char), tmpi, gf);
			}
			{
				int tmpi;
				tmpi = strlen(msg2);
				fwrite(&tmpi, sizeof(int), 1, gf);
				fwrite(msg2, sizeof(char), tmpi, gf);
			}
			{
				int tmpi;
				tmpi = strlen(msg3);
				fwrite(&tmpi, sizeof(int), 1, gf);
				fwrite(msg3, sizeof(char), tmpi, gf);
			}
			fclose(gf);

			/* Call to display the html */
			html_incorrect(url, sessionid, username);
			exit(0);

step_2:

			/* Read local variables from temp file */
			lf = fopen(sessionid, "r");
			fgetc(lf);
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, lf);
				username = (char *)malloc(tmpi+1);
				fread(username, sizeof(char), tmpi, lf);
				username[tmpi] = '\0';
			}
			fread(&correct, sizeof(int), 1, lf);
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, lf);
				password = (char *)malloc(tmpi+1);
				fread(password, sizeof(char), tmpi, lf);
				password[tmpi] = '\0';
			}
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, lf);
				quit = (char *)malloc(tmpi+1);
				fread(quit, sizeof(char), tmpi, lf);
				quit[tmpi] = '\0';
			}
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, lf);
				msg = (char *)malloc(tmpi+1);
				fread(msg, sizeof(char), tmpi, lf);
				msg[tmpi] = '\0';
			}
			fread(&written, sizeof(int), 1, lf);
			fclose(lf);

			/* Read global variables from temp file */
			gf = fopen(gb_name, "r");
			fread(&connections, sizeof(int), 1, gf);
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, gf);
				msg0 = (char *)malloc(tmpi+1) ;
				fread(msg0, sizeof(char), tmpi, gf);
				msg0[tmpi] = '\0';
			}
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, gf);
				msg1 = (char *)malloc(tmpi+1) ;
				fread(msg1, sizeof(char), tmpi, gf);
				msg1[tmpi] = '\0';
			}
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, gf);
				msg2 = (char *)malloc(tmpi+1) ;
				fread(msg2, sizeof(char), tmpi, gf);
				msg2[tmpi] = '\0';
			}
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, gf);
				msg3 = (char *)malloc(tmpi+1) ;
				fread(msg3, sizeof(char), tmpi, gf);
				msg3[tmpi] = '\0';
			}
			fclose(gf);

		}
	}
	copyString(&quit,"no");

	while((strcmp(quit,"yes")!=0))
	{
		
		/* Write local variables into temp file */
		lf = fopen(sessionid, "w");
		fputc(3, lf);
		{
			int tmpi;
			tmpi = strlen(username);
			fwrite(&tmpi, sizeof(int), 1, lf);
			fwrite(username, sizeof(char), strlen(username), lf);
		}
		fwrite(&correct, sizeof(int), 1, lf);
		{
			int tmpi;
			tmpi = strlen(password);
			fwrite(&tmpi, sizeof(int), 1, lf);
			fwrite(password, sizeof(char), strlen(password), lf);
		}
		{
			int tmpi;
			tmpi = strlen(quit);
			fwrite(&tmpi, sizeof(int), 1, lf);
			fwrite(quit, sizeof(char), strlen(quit), lf);
		}
		{
			int tmpi;
			tmpi = strlen(msg);
			fwrite(&tmpi, sizeof(int), 1, lf);
			fwrite(msg, sizeof(char), strlen(msg), lf);
		}
		fwrite(&written, sizeof(int), 1, lf);
		fclose(lf);

		/* Write global variables into temp file */
		gf = fopen(gb_name, "w");
		fwrite(&connections, sizeof(int), 1, gf);
		{
			int tmpi;
			tmpi = strlen(msg0);
			fwrite(&tmpi, sizeof(int), 1, gf);
			fwrite(msg0, sizeof(char), tmpi, gf);
		}
		{
			int tmpi;
			tmpi = strlen(msg1);
			fwrite(&tmpi, sizeof(int), 1, gf);
			fwrite(msg1, sizeof(char), tmpi, gf);
		}
		{
			int tmpi;
			tmpi = strlen(msg2);
			fwrite(&tmpi, sizeof(int), 1, gf);
			fwrite(msg2, sizeof(char), tmpi, gf);
		}
		{
			int tmpi;
			tmpi = strlen(msg3);
			fwrite(&tmpi, sizeof(int), 1, gf);
			fwrite(msg3, sizeof(char), tmpi, gf);
		}
		fclose(gf);

		/* Call to display the html */
		html_Update(url, sessionid, msg0, catString(catString("<A HREF=", msg0), ">"), msg1, catString(catString("<A HREF=", msg1), ">"), msg2, catString(catString("<A HREF=", msg2), ">"));
		exit(0);

step_3:

		/* Read local variables from temp file */
		lf = fopen(sessionid, "r");
		fgetc(lf);
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, lf);
			username = (char *)malloc(tmpi+1);
			fread(username, sizeof(char), tmpi, lf);
			username[tmpi] = '\0';
		}
		fread(&correct, sizeof(int), 1, lf);
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, lf);
			password = (char *)malloc(tmpi+1);
			fread(password, sizeof(char), tmpi, lf);
			password[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, lf);
			quit = (char *)malloc(tmpi+1);
			fread(quit, sizeof(char), tmpi, lf);
			quit[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, lf);
			msg = (char *)malloc(tmpi+1);
			fread(msg, sizeof(char), tmpi, lf);
			msg[tmpi] = '\0';
		}
		fread(&written, sizeof(int), 1, lf);
		fclose(lf);

		/* Read global variables from temp file */
		gf = fopen(gb_name, "r");
		fread(&connections, sizeof(int), 1, gf);
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			msg0 = (char *)malloc(tmpi+1) ;
			fread(msg0, sizeof(char), tmpi, gf);
			msg0[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			msg1 = (char *)malloc(tmpi+1) ;
			fread(msg1, sizeof(char), tmpi, gf);
			msg1[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			msg2 = (char *)malloc(tmpi+1) ;
			fread(msg2, sizeof(char), tmpi, gf);
			msg2[tmpi] = '\0';
		}
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			msg3 = (char *)malloc(tmpi+1) ;
			fread(msg3, sizeof(char), tmpi, gf);
			msg3[tmpi] = '\0';
		}
		fclose(gf);

		copyString(&msg, getField("msg"));
		copyString(&quit, getField("quit"));

		if((strcmp(msg,"")!=0))
		{
			written = written+1;
			copyString(&msg0,msg1);
			copyString(&msg1,msg2);
			copyString(&msg2,msg);
		}
	}
	
	/* Write local variables into temp file */
	lf = fopen(sessionid, "w");
	fputc(3, lf);
	{
		int tmpi;
		tmpi = strlen(username);
		fwrite(&tmpi, sizeof(int), 1, lf);
		fwrite(username, sizeof(char), strlen(username), lf);
	}
	fwrite(&correct, sizeof(int), 1, lf);
	{
		int tmpi;
		tmpi = strlen(password);
		fwrite(&tmpi, sizeof(int), 1, lf);
		fwrite(password, sizeof(char), strlen(password), lf);
	}
	{
		int tmpi;
		tmpi = strlen(quit);
		fwrite(&tmpi, sizeof(int), 1, lf);
		fwrite(quit, sizeof(char), strlen(quit), lf);
	}
	{
		int tmpi;
		tmpi = strlen(msg);
		fwrite(&tmpi, sizeof(int), 1, lf);
		fwrite(msg, sizeof(char), strlen(msg), lf);
	}
	fwrite(&written, sizeof(int), 1, lf);
	fclose(lf);

	/* Write global variables into temp file */
	gf = fopen(gb_name, "w");
	fwrite(&connections, sizeof(int), 1, gf);
	{
		int tmpi;
		tmpi = strlen(msg0);
		fwrite(&tmpi, sizeof(int), 1, gf);
		fwrite(msg0, sizeof(char), tmpi, gf);
	}
	{
		int tmpi;
		tmpi = strlen(msg1);
		fwrite(&tmpi, sizeof(int), 1, gf);
		fwrite(msg1, sizeof(char), tmpi, gf);
	}
	{
		int tmpi;
		tmpi = strlen(msg2);
		fwrite(&tmpi, sizeof(int), 1, gf);
		fwrite(msg2, sizeof(char), tmpi, gf);
	}
	{
		int tmpi;
		tmpi = strlen(msg3);
		fwrite(&tmpi, sizeof(int), 1, gf);
		fwrite(msg3, sizeof(char), tmpi, gf);
	}
	fclose(gf);

	/* We remove session id before exiting */
	remove(sessionid);
	sessionid = "";

	/* Call to display the html */
	html_ByeBye(url, sessionid, username, itoa(connections), itoa(written));
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

	if (strcmp(sessionid,"Addr") == 0) session_Addr(0);
	if (strncmp(sessionid,"Addr$",5) == 0) session_Addr(1);

/* Display the error page if an invalid sessionid is passed */
error_and_exit:
	printf("Content-type: text/html\n\n");
	printf("<html><body>\n");
	printf("<h3>Sorry, your session has expired or is not valid.</h3>\n");
	printf("<h3>Valid session(s) are:</h3>\n");
	printf("   --> Addr<br>");
	printf("<br>Example: <b><i>%s?Addr</i></b>\n", getenv("SCRIPT_NAME"));
	printf("</body></html>\n");
	exit(1);
}
