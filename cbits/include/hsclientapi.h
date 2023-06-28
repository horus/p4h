#ifndef _HS_CLIENT_API_H
#define _HS_CLIENT_API_H

#include "p4/clientapi.h"
#include "specmgr.h"
#include "hsclientapi.h"
#include "hsclientuser.h"
#include <vector>

class HsClientApi
{
public:
	HsClientApi();
	~HsClientApi();

public:
	void ParseSpec(const char *type, const char *form, const char ***k, const char ***v, int *len);
	void Connect();
	void Disconnect();
	bool Dropped() { return client.Dropped(); }
	void Run(const char *cmd) { client.Run(cmd, &ui); }
	void SetArgv(int argc, char *const *argv) { client.SetArgv(argc, argv); }
	void SetInput(const char *i) { ui.SetInput(i); }
	void SetHost(const char *h) { client.SetHost(h); }
	void SetUser(const char *u) { client.SetUser(u); }
	void SetPassword(const char *p) { client.SetPassword(p); }
	void SetPort(const char *p) { client.SetPort(p); }
	void SetClient(const char *c) { client.SetClient(c); }
	StrPtr *GetProtocol(const char *p) { return client.GetProtocol(p); }
	void SetProtocol(const char *var, const char *v) { client.SetProtocol(var, v); }
	void SetProg(const char *p) { client.SetProg(p); }
	void SetVersion(const char *v) { client.SetVersion(v); }
	void SetHandler(const char *meth, void (*fout)(const char *)) { ui.SetHandler(meth, fout); }
	void GetOutput2(const char **msg, const char **err) { ui.GetOutput2(msg, err); }

private:
	const char **CopySv(std::vector<StrRef> &vec);

private:
	ClientApi client;
	HsClientUser ui;
	SpecMgr specMgr;
};

#endif
