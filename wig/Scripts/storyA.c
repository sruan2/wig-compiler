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
char gb_name[] = "story.gb";

FILE *gf;
FILE *lf;

/* Wig global variable section */

char *story = "";
int sentence_count;

/* Wig html section */

void html_Welcome(char *url, char *sessionid, char *story)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("Welcome!  Here is the beginning of a story written by generations of\n    dedicated men and women.  You now have the opportunity to contribute to\n    this great worldwide effort to produce the best story ever.  Unleash\n    your creativity!  Type in the next sentence of the story.");
	printf("</P>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("%s", story);
	printf("</P>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("<input name=\"sentence\" type=\"text\" size=60>");
	printf("</P>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("<input name=\"quit\" type=\"radio\" value=\"yes\">");
	printf("Quit now");
	printf("</P>\n");
	printf("\n    ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

void html_Accept(char *url, char *sessionid, char *story)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("Thank you very much.  Your contribution is very important to the success\n    of this story.  Here is how it looks now.  You may type another sentence at\n    the bottom of this page.");
	printf("</P>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("%s", story);
	printf("</P>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("<input name=\"sentence\" type=\"text\" size=60>");
	printf("</P>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("<input name=\"quit\" type=\"radio\" value=\"yes\">");
	printf("Quit now");
	printf("</P>\n");
	printf("\n    ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

void html_Reject(char *url, char *sessionid, char *story)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("Unfortunately, somebody submitted a sentence before you did.  But you\n    can still write the next sentence.  Try to be quick, this time.  Here is\n    how the story looks like now.");
	printf("</P>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("%s", story);
	printf("</P>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("<input name=\"sentence\" type=\"text\" size=60>");
	printf("</P>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("<input name=\"quit\" type=\"radio\" value=\"yes\">");
	printf("Quit now");
	printf("</P>\n");
	printf("\n    ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

void html_GoodBye(char *url, char *sessionid)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("Thank you!  You are now part of a great work of art!");
	printf("</P>\n");
	printf("\n    ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

void html_ConfirmReset(char *url, char *sessionid)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("<input name=\"confirm\" type=\"radio\" value=\"yes\">");
	printf("\n    \tReally delete the story?");
	printf("</P>\n");
	printf("\n    ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

void html_ResetDone(char *url, char *sessionid)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("OK, the story has been deleted.");
	printf("</P>\n");
	printf("\n    ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

void html_ResetNotDone(char *url, char *sessionid)
{
	printf("Content-type: text/html\n\n");
	printf("<html>\n") ;
	printf("<form method=\"POST\" action=\"%s?%s\">\n", url, sessionid);
	printf("<body>\n");
	printf("\n    ");
	printf("<P>\n");
	printf("The story has not been deleted.");
	printf("</P>\n");
	printf("\n    ");
	printf("</body>\n");
	printf("<br><input type=\"submit\" value=\"Continue\">\n");
	printf("</form><html>\n");
}

/* Wig session section */

void session_Reset(int stage)
{
	char *confirm = "";

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
	sessionid = randomString("Reset", 20);

	/* Read all global variables first */
	gf = fopen(gb_name, "r");
	if (gf != NULL)
	{
		fread(&sentence_count, sizeof(int), 1, gf);
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			story = (char *)malloc(tmpi+1) ;
			fread(story, sizeof(char), tmpi, gf);
			story[tmpi] = '\0';
		}
		fclose(gf);
	}

	
	/* Write local variables into temp file */
	lf = fopen(sessionid, "w");
	fputc(1, lf);
	{
		int tmpi;
		tmpi = strlen(confirm);
		fwrite(&tmpi, sizeof(int), 1, lf);
		fwrite(confirm, sizeof(char), strlen(confirm), lf);
	}
	fclose(lf);

	/* Write global variables into temp file */
	gf = fopen(gb_name, "w");
	fwrite(&sentence_count, sizeof(int), 1, gf);
	{
		int tmpi;
		tmpi = strlen(story);
		fwrite(&tmpi, sizeof(int), 1, gf);
		fwrite(story, sizeof(char), tmpi, gf);
	}
	fclose(gf);

	/* Call to display the html */
	html_ConfirmReset(url, sessionid);
	exit(0);

step_1:

	/* Read local variables from temp file */
	lf = fopen(sessionid, "r");
	fgetc(lf);
	{
		int tmpi;
		fread(&tmpi, sizeof(int), 1, lf);
		confirm = (char *)malloc(tmpi+1);
		fread(confirm, sizeof(char), tmpi, lf);
		confirm[tmpi] = '\0';
	}
	fclose(lf);

	/* Read global variables from temp file */
	gf = fopen(gb_name, "r");
	fread(&sentence_count, sizeof(int), 1, gf);
	{
		int tmpi;
		fread(&tmpi, sizeof(int), 1, gf);
		story = (char *)malloc(tmpi+1) ;
		fread(story, sizeof(char), tmpi, gf);
		story[tmpi] = '\0';
	}
	fclose(gf);

	copyString(&confirm, getField("confirm"));

	if((strcmp(confirm,"yes")==0))
	{
		copyString(&story,"");
		sentence_count = 0;
		
		/* Write local variables into temp file */
		lf = fopen(sessionid, "w");
		fputc(1, lf);
		{
			int tmpi;
			tmpi = strlen(confirm);
			fwrite(&tmpi, sizeof(int), 1, lf);
			fwrite(confirm, sizeof(char), strlen(confirm), lf);
		}
		fclose(lf);

		/* Write global variables into temp file */
		gf = fopen(gb_name, "w");
		fwrite(&sentence_count, sizeof(int), 1, gf);
		{
			int tmpi;
			tmpi = strlen(story);
			fwrite(&tmpi, sizeof(int), 1, gf);
			fwrite(story, sizeof(char), tmpi, gf);
		}
		fclose(gf);

		/* We remove session id before exiting */
		remove(sessionid);
		sessionid = "";

		/* Call to display the html */
		html_ResetDone(url, sessionid);
		exit(0);

step_2:

		/* Read local variables from temp file */
		lf = fopen(sessionid, "r");
		fgetc(lf);
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, lf);
			confirm = (char *)malloc(tmpi+1);
			fread(confirm, sizeof(char), tmpi, lf);
			confirm[tmpi] = '\0';
		}
		fclose(lf);

		/* Read global variables from temp file */
		gf = fopen(gb_name, "r");
		fread(&sentence_count, sizeof(int), 1, gf);
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			story = (char *)malloc(tmpi+1) ;
			fread(story, sizeof(char), tmpi, gf);
			story[tmpi] = '\0';
		}
		fclose(gf);

	}
	else
	{
		
		/* Write local variables into temp file */
		lf = fopen(sessionid, "w");
		fputc(2, lf);
		{
			int tmpi;
			tmpi = strlen(confirm);
			fwrite(&tmpi, sizeof(int), 1, lf);
			fwrite(confirm, sizeof(char), strlen(confirm), lf);
		}
		fclose(lf);

		/* Write global variables into temp file */
		gf = fopen(gb_name, "w");
		fwrite(&sentence_count, sizeof(int), 1, gf);
		{
			int tmpi;
			tmpi = strlen(story);
			fwrite(&tmpi, sizeof(int), 1, gf);
			fwrite(story, sizeof(char), tmpi, gf);
		}
		fclose(gf);

		/* We remove session id before exiting */
		remove(sessionid);
		sessionid = "";

		/* Call to display the html */
		html_ResetNotDone(url, sessionid);
		exit(0);

	}
}

void session_Contribute(int stage)
{
	int mycount;
	char *quit = "", *sentence = "";

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
	sessionid = randomString("Contribute", 20);

	/* Read all global variables first */
	gf = fopen(gb_name, "r");
	if (gf != NULL)
	{
		fread(&sentence_count, sizeof(int), 1, gf);
		{
			int tmpi;
			fread(&tmpi, sizeof(int), 1, gf);
			story = (char *)malloc(tmpi+1) ;
			fread(story, sizeof(char), tmpi, gf);
			story[tmpi] = '\0';
		}
		fclose(gf);
	}

	mycount = sentence_count;
	
	/* Write local variables into temp file */
	lf = fopen(sessionid, "w");
	fputc(1, lf);
	{
		int tmpi;
		tmpi = strlen(sentence);
		fwrite(&tmpi, sizeof(int), 1, lf);
		fwrite(sentence, sizeof(char), strlen(sentence), lf);
	}
	{
		int tmpi;
		tmpi = strlen(quit);
		fwrite(&tmpi, sizeof(int), 1, lf);
		fwrite(quit, sizeof(char), strlen(quit), lf);
	}
	fwrite(&mycount, sizeof(int), 1, lf);
	fclose(lf);

	/* Write global variables into temp file */
	gf = fopen(gb_name, "w");
	fwrite(&sentence_count, sizeof(int), 1, gf);
	{
		int tmpi;
		tmpi = strlen(story);
		fwrite(&tmpi, sizeof(int), 1, gf);
		fwrite(story, sizeof(char), tmpi, gf);
	}
	fclose(gf);

	/* Call to display the html */
	html_Welcome(url, sessionid, story);
	exit(0);

step_1:

	/* Read local variables from temp file */
	lf = fopen(sessionid, "r");
	fgetc(lf);
	{
		int tmpi;
		fread(&tmpi, sizeof(int), 1, lf);
		sentence = (char *)malloc(tmpi+1);
		fread(sentence, sizeof(char), tmpi, lf);
		sentence[tmpi] = '\0';
	}
	{
		int tmpi;
		fread(&tmpi, sizeof(int), 1, lf);
		quit = (char *)malloc(tmpi+1);
		fread(quit, sizeof(char), tmpi, lf);
		quit[tmpi] = '\0';
	}
	fread(&mycount, sizeof(int), 1, lf);
	fclose(lf);

	/* Read global variables from temp file */
	gf = fopen(gb_name, "r");
	fread(&sentence_count, sizeof(int), 1, gf);
	{
		int tmpi;
		fread(&tmpi, sizeof(int), 1, gf);
		story = (char *)malloc(tmpi+1) ;
		fread(story, sizeof(char), tmpi, gf);
		story[tmpi] = '\0';
	}
	fclose(gf);

	copyString(&sentence, getField("sentence"));
	copyString(&quit, getField("quit"));

	while((strcmp(quit,"yes")!=0))
	{

		if(mycount==sentence_count)
		{
			sentence_count = sentence_count+1;
			copyString(&story,catString(story, sentence));
			
			/* Write local variables into temp file */
			lf = fopen(sessionid, "w");
			fputc(2, lf);
			{
				int tmpi;
				tmpi = strlen(sentence);
				fwrite(&tmpi, sizeof(int), 1, lf);
				fwrite(sentence, sizeof(char), strlen(sentence), lf);
			}
			{
				int tmpi;
				tmpi = strlen(quit);
				fwrite(&tmpi, sizeof(int), 1, lf);
				fwrite(quit, sizeof(char), strlen(quit), lf);
			}
			fwrite(&mycount, sizeof(int), 1, lf);
			fclose(lf);

			/* Write global variables into temp file */
			gf = fopen(gb_name, "w");
			fwrite(&sentence_count, sizeof(int), 1, gf);
			{
				int tmpi;
				tmpi = strlen(story);
				fwrite(&tmpi, sizeof(int), 1, gf);
				fwrite(story, sizeof(char), tmpi, gf);
			}
			fclose(gf);

			/* Call to display the html */
			html_Accept(url, sessionid, story);
			exit(0);

step_2:

			/* Read local variables from temp file */
			lf = fopen(sessionid, "r");
			fgetc(lf);
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, lf);
				sentence = (char *)malloc(tmpi+1);
				fread(sentence, sizeof(char), tmpi, lf);
				sentence[tmpi] = '\0';
			}
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, lf);
				quit = (char *)malloc(tmpi+1);
				fread(quit, sizeof(char), tmpi, lf);
				quit[tmpi] = '\0';
			}
			fread(&mycount, sizeof(int), 1, lf);
			fclose(lf);

			/* Read global variables from temp file */
			gf = fopen(gb_name, "r");
			fread(&sentence_count, sizeof(int), 1, gf);
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, gf);
				story = (char *)malloc(tmpi+1) ;
				fread(story, sizeof(char), tmpi, gf);
				story[tmpi] = '\0';
			}
			fclose(gf);

			copyString(&sentence, getField("sentence"));
			copyString(&quit, getField("quit"));
		}
		else
		{
			
			/* Write local variables into temp file */
			lf = fopen(sessionid, "w");
			fputc(3, lf);
			{
				int tmpi;
				tmpi = strlen(sentence);
				fwrite(&tmpi, sizeof(int), 1, lf);
				fwrite(sentence, sizeof(char), strlen(sentence), lf);
			}
			{
				int tmpi;
				tmpi = strlen(quit);
				fwrite(&tmpi, sizeof(int), 1, lf);
				fwrite(quit, sizeof(char), strlen(quit), lf);
			}
			fwrite(&mycount, sizeof(int), 1, lf);
			fclose(lf);

			/* Write global variables into temp file */
			gf = fopen(gb_name, "w");
			fwrite(&sentence_count, sizeof(int), 1, gf);
			{
				int tmpi;
				tmpi = strlen(story);
				fwrite(&tmpi, sizeof(int), 1, gf);
				fwrite(story, sizeof(char), tmpi, gf);
			}
			fclose(gf);

			/* Call to display the html */
			html_Reject(url, sessionid, story);
			exit(0);

step_3:

			/* Read local variables from temp file */
			lf = fopen(sessionid, "r");
			fgetc(lf);
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, lf);
				sentence = (char *)malloc(tmpi+1);
				fread(sentence, sizeof(char), tmpi, lf);
				sentence[tmpi] = '\0';
			}
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, lf);
				quit = (char *)malloc(tmpi+1);
				fread(quit, sizeof(char), tmpi, lf);
				quit[tmpi] = '\0';
			}
			fread(&mycount, sizeof(int), 1, lf);
			fclose(lf);

			/* Read global variables from temp file */
			gf = fopen(gb_name, "r");
			fread(&sentence_count, sizeof(int), 1, gf);
			{
				int tmpi;
				fread(&tmpi, sizeof(int), 1, gf);
				story = (char *)malloc(tmpi+1) ;
				fread(story, sizeof(char), tmpi, gf);
				story[tmpi] = '\0';
			}
			fclose(gf);

			copyString(&sentence, getField("sentence"));
			copyString(&quit, getField("quit"));
		}
	}
	
	/* Write local variables into temp file */
	lf = fopen(sessionid, "w");
	fputc(3, lf);
	{
		int tmpi;
		tmpi = strlen(sentence);
		fwrite(&tmpi, sizeof(int), 1, lf);
		fwrite(sentence, sizeof(char), strlen(sentence), lf);
	}
	{
		int tmpi;
		tmpi = strlen(quit);
		fwrite(&tmpi, sizeof(int), 1, lf);
		fwrite(quit, sizeof(char), strlen(quit), lf);
	}
	fwrite(&mycount, sizeof(int), 1, lf);
	fclose(lf);

	/* Write global variables into temp file */
	gf = fopen(gb_name, "w");
	fwrite(&sentence_count, sizeof(int), 1, gf);
	{
		int tmpi;
		tmpi = strlen(story);
		fwrite(&tmpi, sizeof(int), 1, gf);
		fwrite(story, sizeof(char), tmpi, gf);
	}
	fclose(gf);

	/* We remove session id before exiting */
	remove(sessionid);
	sessionid = "";

	/* Call to display the html */
	html_GoodBye(url, sessionid);
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

	if (strcmp(sessionid,"Reset") == 0) session_Reset(0);
	if (strncmp(sessionid,"Reset$",6) == 0) session_Reset(1);

	if (strcmp(sessionid,"Contribute") == 0) session_Contribute(0);
	if (strncmp(sessionid,"Contribute$",11) == 0) session_Contribute(1);

/* Display the error page if an invalid sessionid is passed */
error_and_exit:
	printf("Content-type: text/html\n\n");
	printf("<html><body>\n");
	printf("<h3>Sorry, your session has expired or is not valid.</h3>\n");
	printf("<h3>Valid session(s) are:</h3>\n");
	printf("   --> Reset<br>");
	printf("<br>Example: <b><i>%s?Reset</i></b>\n", getenv("SCRIPT_NAME"));
	printf("   --> Contribute<br>");
	printf("<br>Example: <b><i>%s?Contribute</i></b>\n", getenv("SCRIPT_NAME"));
	printf("</body></html>\n");
	exit(1);
}
