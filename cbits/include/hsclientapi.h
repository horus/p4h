#ifndef _HS_CLIENT_API_H
#define _HS_CLIENT_API_H

#include "p4/clientapi.h"

class HsClientApi : public ClientApi
{
public:
	HsClientApi();

private:
	ClientApi client;
};

#endif
