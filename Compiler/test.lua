local saludo = "Hola"
local nombre = "Mundo"
print(saludo)
print(nombre)

local a = 10
local b = 5
local suma = a + b
local resta = a - b
local multiplicacion = a * b
local division = a / b
print(suma)
print(resta)
print( multiplicacion)
print(division)

if suma > 10 then
    print("La suma es mayor que 10")
elseif suma == 10 then
    print("La suma es igual a 10")
else
    print("La suma es menor que 10")
end

for i = 1, 5 do
    print(i)
end

local contador = 1
while contador <= 3 do
    print(contador)
    contador = contador + 1
end
