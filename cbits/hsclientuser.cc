#include <string>
#include <cstring>
#include "p4/clientapi.h"
#include "HsFFI.h"
#include "hsclientuser.h"

HsClientUser::HsClientUser(SpecMgr *s)
{
    foutputBinary = nullptr;
    foutputInfo = nullptr;
    foutputMessage = nullptr;
    foutputStat = nullptr;
    foutputText = nullptr;
    specMgr = s;
}

HsClientUser::~HsClientUser()
{
    if (foutputBinary != nullptr)
        hs_free_fun_ptr((HsFunPtr)foutputBinary);
    if (foutputInfo != nullptr)
        hs_free_fun_ptr((HsFunPtr)foutputInfo);
    if (foutputMessage != nullptr)
        hs_free_fun_ptr((HsFunPtr)foutputMessage);
    if (foutputStat != nullptr)
        hs_free_fun_ptr((HsFunPtr)foutputStat);
    if (foutputText != nullptr)
        hs_free_fun_ptr((HsFunPtr)foutputText);
    specMgr = nullptr;
}

void HsClientUser::OutputInfo(char level, const char *data)
{
    size_t len = std::strlen(data);
    if (len == 0)
        return;
    switch (level)
    {
    default:
    case '0':
        break;
    case '1':
        msg.Append("... ");
        break;
    case '2':
        msg.Append("... ... ");
        break;
    }
    msg.Append(data, len);
    msg.Append("\n");

    if (foutputInfo != nullptr)
        foutputInfo(data);
};

void HsClientUser::OutputError(const char *errBuf)
{
    /* called by HandleError */
    err.Set(errBuf);
}

void HsClientUser::Prompt(const StrPtr &msg, StrBuf &rsp, int noEcho, Error *e)
{
    InputData(&rsp, e);
}

void HsClientUser::InputData(StrBuf *strbuf, Error *e)
{
    if (input.Length() > 0)
        strbuf->Set(input);
}

void HsClientUser::SetHandler(const char *meth, void (*fout)(const char *))
{
    std::string method(meth);
    if (method == "outputBinary")
    {
        if (foutputBinary != nullptr)
            hs_free_fun_ptr((HsFunPtr)foutputBinary);
        foutputBinary = fout;
    }
    if (method == "outputInfo")
    {
        if (foutputInfo != nullptr)
            hs_free_fun_ptr((HsFunPtr)foutputInfo);
        foutputInfo = fout;
    }
    if (method == "outputMessage")
    {
        if (foutputMessage != nullptr)
            hs_free_fun_ptr((HsFunPtr)foutputMessage);
        foutputMessage = fout;
    }
    if (method == "outputStat")
    {
        if (foutputStat != nullptr)
            hs_free_fun_ptr((HsFunPtr)foutputStat);
        foutputStat = fout;
    }
    if (method == "outputText")
    {
        if (foutputText != nullptr)
            hs_free_fun_ptr((HsFunPtr)foutputText);
        foutputText = fout;
    }
}

void HsClientUser::SetInput(const char *i)
{
    input.Set(i);
}

void HsClientUser::Finished()
{
    input.Reset();
}

void HsClientUser::GetOutput2(const char **m, const char **e)
{
    *m = DupOutput(msg);
    *e = DupOutput(err);
}

const char *HsClientUser::DupOutput(StrBuf &output)
{
    /*
     * XXX: force terminating with nul char and use real strlen of the underlying buffer
     */
    output.Terminate();
    const char *src = output.Text();
    std::size_t len = std::strlen(src)+1;
    const char *dst;
    if ((dst = (const char *)std::malloc(len)) == nullptr)
        return nullptr;
    std::memcpy((void *)dst, src, len);
    output.Reset();
    return dst; // will be freed by the caller
}
