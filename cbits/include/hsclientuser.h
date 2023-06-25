#ifndef _HS_CLIENT_USER_H
#define _HS_CLIENT_USER_H

class HsClientUser : public ClientUser
{
private:
	StrBuf msg, err;
	char *input;

public:
	HsClientUser();
	// override
	void OutputInfo(char level, const char *data);
	void OutputError(const char *errBuf);
	void InputData(StrBuf *strbuf, Error *e);
	void Prompt(const StrPtr &msg, StrBuf &rsp, int noEcho, Error *e);
	void SetInput(char *i);
	void Finished();
	// helpers
	const char *GetOutput(void);
	const char *GetError(void);
	void GetOutput2(const char **p1, const char **p2);
};

#endif
