#include "p4/stdhdrs.h"
#include "p4/strbuf.h"
#include "p4/error.h"
#include "p4/spec.h"
#include "p4/strdict.h"
#include "p4/strtable.h"
#include "hsspecdata.h"

StrPtr *
HsSpecData::GetLine(SpecElem *sd, int x, const char **cmt)
{
    StrBuf key = sd->tag;
    return sd->IsList() ? dict->GetVar(key, x) : dict->GetVar(key);
}

void HsSpecData::SetLine(SpecElem *sd, int x, const StrPtr *v, Error *e)
{
    StrPtr key = sd->tag;
    sd->IsList() ? dict->SetVar(key, x, *v) : dict->SetVar(key, *v);
}
