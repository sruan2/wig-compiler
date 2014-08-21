#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int isNumeric(char *str);

int main()
{
	char* data = getenv("QUERY_STRING");
	printf("%s%c%c\n","Content-Type:text/html;charset=iso-8859-1",13,10);
	char item[10][50];		//Stores the item names (used temporarily in the following "for loop" to purge part of the parsed string)
	char output[10][50];		// Stores information about which button is checked and what is in the text box
	int prices[5] = {100000,140000,70000,200,100};	// Hard-coded prices
	int maxStock[5] = {210,5,200,34000,400};	// Hard-coded stock limits
	int howManyBought[5];				// Will store how many items are actually being bought (will not go lower than maxStock)
	int individualTotal[5];				// Stores sub-total prices
	int isChecked[5]={0,0,0,0,0};			// Stores whether the item is checked (it needs to be, or won't be considered)
	int index = 0;
	int i;

	// Here is where all the parsing is being done
	for (i=0; i<10; i++){
		if(*(data+index)=='b'){ // This is a little trick. My only input (out of all my radio buttons, checkbox and textbox, the only one starting with 'b' is the checkbox)
			isChecked[i/2]=1; 	// Add a boolean value of 1 for this item
			index += strlen("buyIt=X&"); // Adjust the counter
		}
		sscanf(data + index,"%[^'=']=%[^'&']",item[i],output[i]); // Parse the input in QUERY_STRING. Only the output[i] is kept
		index += strlen(item[i]) + strlen(output[i]) + 2;		// Move our cursor
	}
	
	// Hard-code item names
	strcpy(item[0],"E.T.");
	strcpy(item[1],"STITCH");
	strcpy(item[2],"HYDRALISK");
	strcpy(item[3],"ALIEN FOOD");
	strcpy(item[4],"FANCY ALIEN TREAT");

	int howMany; // Temporary variable used when parsing strings with atoi

	printf("<html><head><link rel=\"shortcut icon\" href=\"../../~tparfe1/images/hulastitch.gif\"><body bgcolor=#CCFFB2><title>Your bill</title></head><body><CENTER><table border=1 width=600 bgcolor=#99FF66><tr><td><FONT FACE=Courier New><br>Alien Pet Shop -- \"One stop shop for all your alien needs\"<CENTER><br><br><br>");
	printf("Receipt<br><br>");	

	 // Important note about output[i]: Even (including 0) indices hold information regarding which button is selected. Odd indices hold information regarding how many items are selected.
	
	for (i=0; i<5; i++){
		howMany = atoi(output[2*i+1]);
		if (isChecked[i]==0 && (strcmp(output[2*i],"one")==0 || strcmp(output[2*i],"all")==0 || howMany!=0)){
			printf("<B>You must check %s if you wish to purchase it</B><br><br>",item[i]);
			individualTotal[i]=-1;
			continue;
		}
		if (howMany > maxStock[i]){	// Check if the user asked for too many copies of an item
			printf("<B>Your request of %d %s exceeded our stock. You are being billed for as many items as we can sell you</B><br>", howMany, item[i]);
			howMany = maxStock[i];
		}
					  
		if (strcmp(output[2*i],"one")==0){	// Check if he chose exactly one
			printf("You have selected 1 %s<br><br>", item[i]);
			howManyBought[i] = 1;
			individualTotal[i] = prices[i];
		}else if (strcmp(output[2*i],"all")==0){ // Check if he chose all of them
			printf("You have selected %d %s<br><br>", maxStock[i], item[i]); 
			howManyBought[i] = maxStock[i];
			individualTotal[i] = maxStock[i]*prices[i];
		}else if(isNumeric(output[2*i+1])==1){  // Check if he chose a specific amount (as long as it is numerical)
			if (strcmp(output[2*i+1],"0")!=0){ // If he chose anything else than 0
				printf("You have selected %d %s<br><br>", howMany, item[i]);
				howManyBought[i] = howMany;
				individualTotal[i] = howMany*prices[i];
			}else{
				individualTotal[i] = -1; // He chose 0 items. We store -1 to say that he didn't choose that item.
			}
		}else{
			printf("Invalid request for %s<br><br>", item[i]); // Invalid input. Perhaps it wasn't numerical
			individualTotal[i] = -1;
		}
	}
	printf("----------------------------</CENTER><br><br>");
	int totalPrice = 0;
	double final, GST, PST;	

	for (i=0; i<5; i++){ // Add all prices, where individualTotal isn't -1: -1 refers to invalid input or 0 items selected
		if (individualTotal[i] != -1){
			printf("%s * %d, %d$ per unit<B><P ALIGN=right>%d$</P></B>", item[i], howManyBought[i], prices[i], individualTotal[i]);
			totalPrice += individualTotal[i];
		}
	}

	// Get the GST/PST and total price
	GST = totalPrice*0.05;
	PST = (totalPrice+GST)*0.095;
	final = totalPrice + GST + PST;

	printf("<B><P ALIGN=right>SUBTOTAL : %.2f$<br>", (float)totalPrice);
	printf("GST : %.2f$<br>", GST);
	printf("PST : %.2f$<br>", PST);
	printf("PAYMENT : %.2f$<br<br><br></B></P>",final);


	printf("<br><br><br></FONT></td></tr></table><br>");

	printf("<form method=link action=\"./store.html\"><input type=submit value=\"Back to the shop\"></form>");
	printf("</CENTER></body></html>");
}

// Takes as input a string and refers whether it is numerical, by going through all characters one by one and checking them
int isNumeric(char *str)
{
	while(*str){
		if (!isdigit(*str)){
			return 0;
		}
		str++;
	}
	return 1;
}

