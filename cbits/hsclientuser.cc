#include "p4/clientapi.h"
#include "HsFFI.h"
#include "hsclientuser.h"

HsClientUser::HsClientUser()
{
    foutputBinary = nullptr;
    foutputInfo = nullptr;
    foutputMessage = nullptr;
    foutputStat = nullptr;
    foutputText = nullptr;
    input = 0;
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
    if (input != nullptr)
        free(input);
}

void HsClientUser::OutputInfo(char level, const char *data)
{
    size_t len = strlen(data);
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
    if (!input)
        return;
    strbuf->Set(input);
}

void HsClientUser::SetHandler(const char *method, void (*fout)(const char *))
{
    if (strcmp(method, "outputBinary") == 0)
    {
        if (foutputBinary != nullptr)
            hs_free_fun_ptr((HsFunPtr)foutputBinary);
        foutputBinary = fout;
    }
    if (strcmp(method, "outputInfo") == 0)
    {
        if (foutputInfo != nullptr)
            hs_free_fun_ptr((HsFunPtr)foutputInfo);
        foutputInfo = fout;
    }
    if (strcmp(method, "outputMessage") == 0)
    {
        if (foutputMessage != nullptr)
            hs_free_fun_ptr((HsFunPtr)foutputMessage);
        foutputMessage = fout;
    }
    if (strcmp(method, "outputStat") == 0)
    {
        if (foutputStat != nullptr)
            hs_free_fun_ptr((HsFunPtr)foutputStat);
        foutputStat = fout;
    }
    if (strcmp(method, "outputText") == 0)
    {
        if (foutputText != nullptr)
            hs_free_fun_ptr((HsFunPtr)foutputText);
        foutputText = fout;
    }
}

void HsClientUser::SetInput(char *i)
{
    if (NULL != input)
        free(input);
    input = strdup(i);
}

void HsClientUser::Finished()
{
    if (input) {
        free(input);
        input = nullptr;
    }
}

void HsClientUser::GetOutput2(const char **m, const char **e)
{
    *m = DupOutput(msg);
    *e = DupOutput(err);
}

char *HsClientUser::DupOutput(StrBuf &output)
{
    /*
     * XXX: force terminating with nul char and use real strlen of the underlying buffer
     */
    output.Terminate();
    char *src = output.Text();
    size_t len = strlen(src);
    char *dst;
    if ((dst = (char *)malloc(len + 1)) == nullptr)
        return nullptr;
    memcpy(dst, src, len);
    dst[len] = '\0';
    output.Reset();
    return dst; // will be freed by the caller
}
