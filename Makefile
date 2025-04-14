##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## Makefile for building GLaDOS
##

NAME=glados

STACK_PATH=stack

$(NAME):
	$(STACK_PATH) install --local-bin-path .

all: $(NAME)

clean:
	rm -f src/Main

fclean: clean
	rm -f $(NAME)

re: fclean all

tests_run:
	$(STACK_PATH) test --coverage

.PHONY:
	all clean fclean re tests_run
