#ifndef _HS_SPEC_DATA_H
#define _HS_SPEC_DATA_H

class HsSpecData : public SpecData
{
public:
    HsSpecData(StrDict *d) { dict = d; }

    virtual StrPtr *GetLine(SpecElem *sd, int x, const char **cmt);
    virtual void SetLine(SpecElem *sd, int x, const StrPtr *val, Error *e);

private:
    StrDict *dict;
};

#endif
