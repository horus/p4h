#ifndef _HS_CLIENT_USER_H
#define _HS_CLIENT_USER_H

class HsClientUser : public ClientUser
{
private:
	StrBuf msg, err, input;

	void (*foutputBinary)(const char *);
	void (*foutputInfo)(const char *);
	void (*foutputMessage)(const char *);
	void (*foutputStat)(const char *);
	void (*foutputText)(const char *);

public:
	HsClientUser();
	~HsClientUser();
	// override
	void OutputInfo(char level, const char *data);
	void OutputError(const char *errBuf);
	void InputData(StrBuf *strbuf, Error *e);
	void Prompt(const StrPtr &msg, StrBuf &rsp, int noEcho, Error *e);
	void SetInput(const char *i);
	void Finished();
	// helpers
	void SetHandler(const char *meth, void (*fout)(const char *));
	void GetOutput2(const char **m, const char **e);

private:
	const char *DupOutput(StrBuf &output);
};

#endif
