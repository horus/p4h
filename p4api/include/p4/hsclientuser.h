#ifndef _HS_CLIENT_USER_H
#define _HS_CLIENT_USER_H

class HsClientUser : public ClientUser
{
private:
	StrBuf msg, err;

public:
	// override
	void OutputInfo(char level, const char *data)
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

	void OutputError(const char *errBuf)
	{
		err.Set(errBuf);
	}

	// helpers
	const char *GetOutput(void)
	{
		msg.Terminate();
		const char *p = strdup(msg.Text());
		msg.Clear();
		return p;
	};

	const char *GetError(void)
	{
		err.Terminate();
		const char *p = strdup(err.Text());
		err.Clear();
		return p;
	};

	void GetOutput2(const char **p1, const char **p2)
	{
		msg.Terminate();
		*p1 = strdup(msg.Text());
		msg.Clear();

		err.Terminate();
		*p2 = strdup(err.Text());
		err.Clear();
	};
};

#endif
