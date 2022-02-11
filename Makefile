##
## EPITECH PROJECT, 2021
## Makefile
## File description:
## Makefile
##

RM		=	rm -f

NAME		=	funEvalExpr

all:
	stack build
	cp $(shell stack path --local-install-root)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

re:	fclean all

.PHONY: all clean fclean re
