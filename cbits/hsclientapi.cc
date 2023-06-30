#include "hsclientapi.h"
#include <cstdlib>
#include <cstring>

HsClientApi::HsClientApi() : ui(&specMgr)
{
    // Enable form parsing
    InitFlags();
    server2 = 0;
    client.SetProtocol("specstring", "");
}

HsClientApi::~HsClientApi()
{
    if (IsConnected())
    {
        Error e;
        client.Final(&e); // Ignore errors
        ResetFlags();
    }
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

const char *
HsClientApi::FormatSpec(const char *type, const char *k[], const char *v[], int len)
{
    if (!specMgr.HaveSpecDef(type))
        return nullptr;
    StrDict *sd = new StrBufDict;
    for (int i = 0; i < len; i++)
    {
        StrBuf key = k[i];
        StrBuf val = v[i];
        sd->SetVar(key, val);
    }
    StrBuf out;
    Error e;
    specMgr.SpecToString(type, sd, out, &e);
    delete sd;
    if (e.Test())
        return nullptr;
    return DupOutput(out);
}

const char **HsClientApi::CopySv(std::vector<StrRef> &vec)
{
    std::size_t sz = vec.size();
    const char **p = (const char **)std::malloc(sizeof(const char *) * sz);
    if (p != nullptr)
        for (std::size_t i = 0; i < sz; i++)
        {
            auto s = vec[i].Text();
            std::size_t len = std::strlen(s) + 1;
            if ((p[i] = (const char *)std::malloc(len)) == nullptr)
            {
                for (int j = i - 1; j >= 0; j--)
                    std::free((void *)(p[j]));
                std::free(p);
                return nullptr;
            }
            std::memcpy((void *)(p[i]), s, len);
        }
    return p;
}

void HsClientApi::Connect()
{
    if (IsConnected())
        return;
    ResetFlags();
    Error e;
    client.Init(&e);
    if (e.Test())
    {
        ui.HandleError(&e);
        return;
    }
    SetConnected();
}

void HsClientApi::Disconnect()
{
    if (!IsConnected())
        return;
    Error e;
    client.Final(&e);
    ResetFlags();
    if (e.Test())
        ui.HandleError(&e);
    specMgr.Reset();
}

bool HsClientApi::Dropped()
{
    if (IsConnected() && !client.Dropped())
        return false;
    if (IsConnected())
        Disconnect();
    return true;
}

void HsClientApi::Run(const char *cmd, const char **msg, const char **err)
{
    if (IsConnected())
    {
        client.Run(cmd, &ui);
        if (!IsCmdRun())
        {
            // Have to request server2 protocol *after* a command has been run. I
            // don't know why, but that's the way it is.
            StrPtr *pv;
            if ((pv = client.GetProtocol("server2")))
                server2 = pv->Atoi();
            if ((pv = client.GetProtocol(P4Tag::v_nocase)))
                SetCaseFold();
            if ((pv = client.GetProtocol(P4Tag::v_unicode)))
                if (pv->Atoi())
                    SetUnicode();
        }
        SetCmdRun();
    }
    else
    {
        Error e;
        e.Set(E_FAILED, "server not connected");
        ui.HandleError(&e);
    }
    ui.GetOutput2(msg, err);
}

const char *
HsClientApi::DupOutput(StrBuf &output)
{
    /*
     * XXX: force terminating with nul char and use real strlen of the underlying buffer
     */
    output.Terminate();
    const char *src = output.Text();
    std::size_t len = std::strlen(src) + 1;
    const char *dst;
    if ((dst = (const char *)std::malloc(len)) == nullptr)
        return nullptr;
    std::memcpy((void *)dst, src, len);
    output.Reset();
    return dst; // will be freed by the caller
}
