#include <libjson/libjson.h>
#include <Rdefines.h>

SEXP processJSONNode(JSONNODE *node, int parentType);

SEXP
R_fromJSON(SEXP r_str)
{
    const char * str = CHAR(STRING_ELT(r_str, 0));
    JSONNODE *node;
    SEXP ans;
    node = json_parse(str);
    ans = processJSONNode(node, json_type(node));
    json_delete(node);
    return(ans);
}

SEXP 
processJSONNode(JSONNODE *n, int parentType)
{
    if (n == NULL){
        PROBLEM "invalid JSON input"
	    ERROR;
    }
 
    JSONNODE *i;
    int len = 0, ctr = 0;
    int nprotect = 0;
    len = json_size(n);
    char startType = parentType; // was 127
    


    SEXP ans, names = NULL;
    PROTECT(ans = NEW_LIST(len)); nprotect++;

    int homogeneous = 0;
    while (ctr < len){  // i != json_end(n)

	i = json_at(n, ctr);

        if (i == NULL){
            PROBLEM "Invalid JSON Node"
		ERROR;
        }

	json_char *node_name = json_name(i);
	
	char type = json_type(i);
	if(startType == 127)
	    startType = type;

	SEXP el;
	switch(type) {
   	   case JSON_NULL:
	       el = R_NilValue;
	       break;
   	   case JSON_ARRAY:
  	   case JSON_NODE:
	       el = processJSONNode(i, type);
	       break;
 	   case JSON_NUMBER:
	       el = ScalarReal(json_as_float(i));
	       homogeneous++;
	       break;
 	   case JSON_BOOL:
	       el = ScalarLogical(json_as_bool(i));
	       break;
 	   case JSON_STRING:
	   {
//XXX Garbage collection
	       char *tmp = json_as_string(i);
                   // do we need to strdup here?
	       el = ScalarString(mkChar(tmp));
	       json_free(tmp);
	   }
	       break;
	default:
	    PROBLEM "shouldn't be here"
		WARN;
	    el = R_NilValue;
	    break;
	}
	SET_VECTOR_ELT(ans, ctr, el);

	if(parentType == JSON_NODE || (node_name && node_name[0])) {
	    if(names == NULL) {
	        PROTECT(names = NEW_CHARACTER(len)); nprotect++;
	    }
	    if(node_name && node_name[0])
		SET_STRING_ELT(names, ctr, mkChar(node_name));
	}
	json_free(node_name);
	ctr++;
    }

    /* If we have an empty object, we try to make it into a form equivalent to emptyNamedList
       if it is a {},  or as an AsIs object in R if an empty array. */
    if(len == 0 && (parentType == -1 || parentType == JSON_ARRAY || parentType == JSON_NODE)) {
        if(parentType == -1) 
            parentType = startType;

        if(parentType == JSON_NODE)
	   SET_NAMES(ans, NEW_CHARACTER(0));
        else  {
	   SET_CLASS(ans, ScalarString(mkChar("AsIs")));
	}

    } else {
         if(homogeneous == len) {
      	SEXP tmp;
             PROTECT(tmp = NEW_NUMERIC(len)); nprotect++;
      	for(ctr = 0; ctr < len; ctr++)
      	    REAL(tmp)[ctr] = REAL(VECTOR_ELT(ans, ctr))[0];
      	ans = tmp;
         }
    }
       

    if(names)
	SET_NAMES(ans, names);
	
    UNPROTECT(nprotect);
    return(ans);
}


SEXP
R_libjson_version()
{
    char buf[20];
    sprintf(buf, "%d.%d-%d", (int) __LIBJSON_MAJOR__,
    	                     (int) __LIBJSON_MINOR__,
	                     (int) __LIBJSON_PATCH__);

    return(ScalarString(mkChar(buf)));
}
