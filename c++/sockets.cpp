void MainWindow::createServerSocket()
{
    // INET - IPv4 protocol, STREAM - tcp connection
    socketfd = socket(AF_INET, SOCK_STREAM, 0);
    if (socketfd == -1) {
        printf("[!] socket connection failed\n");
        return ;
    }

    // reuse address and port
    setsockopt(socketfd, , , ,);


    struct sockaddr_in addr;
    int addrlen = sizeof(addr);
    //
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons( PORT );

    if (bind(sockfd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        perror("bind failed");
        sockfd = -1;
        return ;
    }
    if (listen(sockfd, 1) < 0) // listen for 1 connection
    {
        perror("listen failed");
        sockfd = -1;
        return ;
    }
    int conn;
    if ((conn = accept(sockfd, &addr, (socklen_t*)&addrlen)) < 0) {
        perror("accept failed");
        return ;
    }
    int len = read(conn, buffer, 1024);
    // TODO - implement connection handler
}

void MainWindow::createClientSocket()
{
    // INET - IPv4 protocol, STREAM - tcp connection
    socketfd = socket(AF_INET, SOCK_STREAM, 0);
    if (socketfd == -1) {
        printf("[!] socket connection failed\n");
        return ;
    }

    struct sockaddr addr;
    socklen_t addrlen;
    int conn = connect(sockfd, &addr, addrlen);
    // TODO - implement connection handler
}
