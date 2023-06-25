#include "p4/clientapi.h"
#include "hsclientuser.h"

HsClientUser::HsClientUser()
{
    input = 0;
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
};

void HsClientUser::OutputError(const char *errBuf)
{
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

void HsClientUser::SetInput(char *i)
{
    if (NULL != input)
        free(input);
    input = strdup(i);
}

void HsClientUser::Finished()
{
    if (input)
        free(input);
}

const char *HsClientUser::GetOutput(void)
{
    msg.Terminate();
    const char *p = strdup(msg.Text());
    msg.Clear();
    return p;
}

const char *HsClientUser::GetError(void)
{
    err.Terminate();
    const char *p = strdup(err.Text());
    err.Clear();
    return p;
}

void HsClientUser::GetOutput2(const char **p1, const char **p2)
{
    msg.Terminate();
    *p1 = strdup(msg.Text());
    msg.Clear();

    err.Terminate();
    *p2 = strdup(err.Text());
    err.Clear();
}
