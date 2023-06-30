#ifndef _HS_CLIENT_API_H
#define _HS_CLIENT_API_H

#include "p4/clientapi.h"
#include "p4/stdhdrs.h"
#include "p4/strtable.h"
#include "specmgr.h"
#include "hsclientuser.h"
#include <vector>

class HsClientApi
{
public:
	HsClientApi();
	~HsClientApi();

public:
	void Connect();
	void Disconnect();
	bool Dropped();
	void Run(const char *cmd, const char **msg, const char **err);
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

public:
	void ParseSpec(const char *type, const char *form, const char ***k, const char ***v, int *len);
	const char *FormatSpec(const char *type, const char *k[], const char *v[], int len);

private:
	enum
	{
		S_TAGGED = 0x0001,
		S_CONNECTED = 0x0002,
		S_CMDRUN = 0x0004,
		S_UNICODE = 0x0008,
		S_CASEFOLDING = 0x0010,
		S_TRACK = 0x0020,
		S_STREAMS = 0x0040,
		S_GRAPH = 0x0080,

		S_INITIAL_STATE = 0x00C1, // Streams, Graph, and Tagged enabled by default
		S_RESET_MASK = 0x001E,
	};

private:
	void InitFlags() { flags = S_INITIAL_STATE; }
	void ResetFlags() { flags &= ~S_RESET_MASK; }

	void SetTag() { flags |= S_TAGGED; }
	void ClearTag() { flags &= ~S_TAGGED; }
	int IsTag() { return flags & S_TAGGED; }

	void SetConnected() { flags |= S_CONNECTED; }
	void ClearConnected() { flags &= ~S_CONNECTED; }
	int IsConnected() { return flags & S_CONNECTED; }

	void SetCmdRun() { flags |= S_CMDRUN; }
	void ClearCmdRun() { flags &= ~S_CMDRUN; }
	int IsCmdRun() { return flags & S_CMDRUN; }

	void SetUnicode() { flags |= S_UNICODE; }
	void ClearUnicode() { flags &= ~S_UNICODE; }
	int IsUnicode() { return flags & S_UNICODE; }

	void SetCaseFold() { flags |= S_CASEFOLDING; }
	void ClearCaseFold() { flags &= ~S_CASEFOLDING; }
	int IsCaseFold() { return flags & S_CASEFOLDING; }

	void SetTrackMode() { flags |= S_TRACK; }
	void ClearTrackMode() { flags &= ~S_TRACK; }
	int IsTrackMode() { return flags & S_TRACK; }

	void SetStreamsMode() { flags |= S_STREAMS; }
	void ClearStreamsMode() { flags &= ~S_STREAMS; }
	int IsStreamsMode() { return flags & S_STREAMS; }

	void SetGraphMode() { flags |= S_GRAPH; }
	void ClearGraphMode() { flags &= ~S_GRAPH; }
	int IsGraphMode() { return flags & S_GRAPH; }

private:
	const char **CopySv(std::vector<StrRef> &vec);
	const char *DupOutput(StrBuf &output);

private:
	ClientApi client;
	HsClientUser ui;
	SpecMgr specMgr;
	int flags;
	int server2;
};

#endif
