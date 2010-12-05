#include <libjson.h>
#include <Rdefines.h>

SEXP processJSONNode(JSONNODE *node);

SEXP
R_fromJSON(SEXP r_str)
{
    const char * str = CHAR(STRING_ELT(r_str, 0));
    JSONNODE *node;
    node = json_parse(str);

    return(processJSONNode(node));
}

SEXP 
processJSONNode(JSONNODE *n)
{
    if (n == NULL){
        PROBLEM "invalid JSON input"
	    ERROR;
    }
 
    JSONNODE *i;
    int len = 0, ctr = 0;
    int nprotect = 0;
    len = json_size(n);
    


    SEXP ans, names = NULL;
    PROTECT(ans = NEW_LIST(len)); nprotect++;

    int homogeneous = 0;
    while (ctr < len){  // i != json_end(n)

	i = json_at(n, ctr);

	json_char *node_name = json_name(i);

        if (i == NULL){
            PROBLEM "Invalid JSON Node"
		ERROR;
        }
	
	char type = json_type(i);
	SEXP el;
	switch(type) {
   	   case JSON_NULL:
	       el = R_NilValue;
	       break;
   	   case JSON_ARRAY:
  	   case JSON_NODE:
	       el = processJSONNode(i);
	       break;
 	   case JSON_NUMBER:
	       el = ScalarReal(json_as_float(i));
	       homogeneous++;
	       break;
 	   case JSON_BOOL:
	       el = ScalarLogical(json_as_bool(i));
	       break;
 	   case JSON_STRING:
	       el = ScalarString(mkChar(json_as_string(i)));
	       break;
	}
	SET_VECTOR_ELT(ans, ctr, el);

	if(node_name && node_name[0]) {
	    if(names == NULL) {
	        PROTECT(names = NEW_CHARACTER(len)); nprotect++;
	    }
	    SET_STRING_ELT(names, ctr, mkChar(node_name));
	    json_free(node_name);
	}
	ctr++;
    }
    if(homogeneous == len) {
	SEXP tmp;
        PROTECT(tmp = NEW_NUMERIC(len)); nprotect++;
	for(ctr = 0; ctr < len; ctr++)
	    REAL(tmp)[ctr] = REAL(VECTOR_ELT(ans, ctr))[0];
	ans = tmp;
    }
       

    if(names)
	SET_NAMES(ans, names);
	
    UNPROTECT(nprotect);
    return(ans);
}
