#include "hsclientapi.h"

HsClientApi::HsClientApi() : ui(&specMgr)
{
    // Enable form parsing
    client.SetProtocol("specstring", "");
}

HsClientApi::~HsClientApi()
{
}

void HsClientApi::ParseSpec(const char *type, const char *form, const char ***pk, const char ***pv, int *len)
{
    Error e;
    StrDict *spec = specMgr.StringToSpec(type, form, &e);
    if (e.Test())
    {
        ui.HandleError(&e);
        return;
    }
    if (spec != nullptr)
    {
        // StrBuf sb;
        // specMgr.SpecToString(type, spec, sb, &e);
        // std::cout << "[output]" << std::endl << sb.Text()<< std::endl;
        // StrPtr *sb1 = spec->GetVar(type);
        std::vector<StrRef> vecKey;
        std::vector<StrRef> vecVal;
        StrRef var, val;
        int i;
        for (i = 0; spec->GetVar(i, var, val); i++)
        {
            vecKey.push_back(var);
            vecVal.push_back(val);
        }
        *len = i;
        *pk = CopySv(vecKey);
        *pv = CopySv(vecVal);
    }
}

const char **HsClientApi::CopySv(std::vector<StrRef> &vec)
{
    size_t sz = vec.size();
    const char **p = (const char **)malloc(sizeof(const char *) * sz);
    if (p != nullptr)
        for (int i = 0; i < sz; i++)
        {
            auto s = vec[i].Text();
            size_t len = std::strlen(s) + 1;
            p[i] = (const char *)malloc(len);
            memcpy((void *)(p[i]), s, len);
        }
    return p;
}

void HsClientApi::Connect()
{
    Error e;
    client.Init(&e);
    if (e.Test())
        ui.HandleError(&e);
}

void HsClientApi::Disconnect()
{
    Error e;
    client.Final(&e);
    if (e.Test())
        ui.HandleError(&e);
    specMgr.Reset();
}
