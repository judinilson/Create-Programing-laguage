import lang

while True:
    text = input('lang > ')
    result, error = lang.run('<stdin>', text)

    if error:
        print(error.as_string())
    elif result:
        print(repr(result))
