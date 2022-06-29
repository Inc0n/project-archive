
CC=gxc
CC_LIB=$(CC)
CC_BIN=$(CC) -exe
CFLAGS=-O

NAME=you-get
GERBIL_BIN_PATH=~/.gerbil/bin/

LIB_SRC=utils.ss common.ss
BIN_SRC=you-get.ss

all:
	$(CC_LIB) $(CFLAGS) $(LIB_SRC)
	$(CC_BIN) $(CFLAGS) -o $(GERBIL_BIN_PATH)/$(NAME) $(BIN_SRC)
	strip $(GERBIL_BIN_PATH)/$(NAME)
	ln -s $(GERBIL_BIN_PATH)/$(NAME) $(NAME)
# ./build.ss

test-kuwo:
	./$(NAME) get www.kuwo.cn/play_detail/123