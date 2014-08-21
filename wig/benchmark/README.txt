Readme File 

Group A: David Thibodeau, Shanshan Ruan, Ioannis Fytilis

WIG service:

You can access it at http://www.cs.mcgill.ca/~sruan2/cgi-bin/GroupA.cgi?Start

There is one session: Start

Our WIG service is a matrix calculator which can perform three types of matrix calculation 

for any 3 * 3 integer-valued matrix. They are: determinant calculation, trace calculation, and 

transpose matrix display. Moreover, the user can always reset the matrix and enter the different 

data to perform new calculation. When the user wants to finish the service, he can simply select 

“quit” button and exit the WIG service.

Some specifications:

(1) The WIG service can only recognize integers. It will replace all invalid input with zero.

(2) If the user leaves some input field blank, our matrix calculator will fill the void with zero 

automatically.

CGI script:

We provide a manually compiled cgi script at: http://cs.mcgill.ca/~ifytil/store.html

(Well, you actually need to "buy" something for the CGI script to execute)

General compilation idea:

The compiler first does the lexcial analysis to extract tokens from the wig source file, then it

parses and creates the syntax tree based on the syntax provided. Suppose the target file is in C

code, the compiler should produce a c file which generates (print to stdout) dynamic html code 

for each "const html" statement. It should also translate "plug" and "receive" to appropriate 

html forms which can support data communation between the client and the server. The generated c file

also needs to implement all necessary functions to help process data obtained from the form. To help 

the user avoid further compilation of c file, we can also generate a script which does the automatic 

compilation and change the cgi to an excutable for the user.
