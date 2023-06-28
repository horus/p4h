#include "hsclientapi.h"
#include <iostream>

HsClientApi::HsClientApi() : ui(&specMgr)
{
    // Enable form parsing
    client.SetProtocol( "specstring", "" );
}

HsClientApi::~HsClientApi()
{
}

void HsClientApi::ParseSpec(const char *type, const char *form)
{
        Error e;
        StrDict *spec = specMgr.StringToSpec(type, form, &e);
        if (e.Test()) {
                ui.HandleError(&e);
                return;
        }
        std::cout << "parsed" << std::endl;
        return;

}

void HsClientApi::Connect()
{
    Error e;
    client.Init(&e);
    if (e.Test())
        ui.HandleError(&e);
}

void HsClientApi::Disconnect()
{
    Error e;
    client.Final(&e);
    if (e.Test())
        ui.HandleError(&e);
    specMgr.Reset();
}
