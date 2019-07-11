#include "EventHandler.h"

using namespace May;

EventHandler::~EventHandler()
{
    if (m_fd != FD_NOT_SET)
    {
        close(m_fd);
        m_fd = FD_NOT_SET;
    }
}

bool
EventHandler::Init()
{
    m_fd = epoll_create(EPOLL_SIZE);

    if (m_fd == FD_NOT_SET)
    {
        return false;
    }

    return true;
}