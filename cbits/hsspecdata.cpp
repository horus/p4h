/*******************************************************************************

Copyright (c) 2010, Perforce Software, Inc.  All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1.  Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

2.  Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL PERFORCE SOFTWARE, INC. BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*******************************************************************************/

/*******************************************************************************
 * Name		: PythonSpecData.cpp
 *
 * Author	: Sven Erik Knop <sknop@perforce.com>
 *
 * Description	: Python bindings for the Perforce API. SpecData subclass for
 * 		  P4Python. This class allows for manipulation of Spec data
 * 		  stored in a Python dict using the standard Perforce classes
 *
 ******************************************************************************/

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
