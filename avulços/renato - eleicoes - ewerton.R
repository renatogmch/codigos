utils::memory.limit(32000)

#Informações
n_candidatos = 5
n_partidos = 3
q_eleitoral = 4
clausula = 0.1*q_eleitoral


#Criando matrix de possibilidades de filiação
b = c(1:n_partidos)
e = lapply(seq_len(n_candidatos), function(X) b)
A = expand.grid(e)

#Votos esperados de cada candidato
vot = data.frame(voto = c(4,3,5,2,6))
rownames(vot) = colnames(A)

A[A==1] = "PA"
A[A==2] = "PB"
A[A==3] = "PC"

#calculo do numero de eleitos do partido A
a = A=="PA"
a[1,] = as.numeric(a[1,])
b = as.matrix(vot)
votos.por.partido = a%*%b
q_partido = votos.por.partido/q_eleitoral
d = a

for (i in c(1:nrow(d))) {
  d[i,] = a[i,]*b
}

numero.eleitos.A = votos.por.partido
for (i in 1:nrow(votos.por.partido)) {
  numero.eleitos.A[i,] = min(floor(q_partido[i,]),length(d[i,][d[i,]>clausula]))
}
colnames(numero.eleitos.A) = "eleitos.A"
#calculo do numero de eleitos do partido B
a = A=="PB"
a[1,] = as.numeric(a[1,])
b = as.matrix(vot)
votos.por.partido = a%*%b
q_partido = votos.por.partido/q_eleitoral
d = a

for (i in c(1:nrow(d))) {
  d[i,] = a[i,]*b
}

numero.eleitos.B = votos.por.partido
for (i in 1:nrow(votos.por.partido)) {
  numero.eleitos.B[i,] = min(floor(q_partido[i,]),length(d[i,][d[i,]>clausula]))
}
colnames(numero.eleitos.B) = "eleitos.B"
#calculo do numero de eleitos do partido C
a = A=="PC"
a[1,] = as.numeric(a[1,])
b = as.matrix(vot)
votos.por.partido = a%*%b
q_partido = votos.por.partido/q_eleitoral
d = a

for (i in c(1:nrow(d))) {
  d[i,] = a[i,]*b
}

numero.eleitos.C = votos.por.partido
for (i in 1:nrow(votos.por.partido)) {
  numero.eleitos.C[i,] = min(floor(q_partido[i,]),length(d[i,][d[i,]>clausula]))
}
colnames(numero.eleitos.C) = "eleitos.C"
#Unificando

M = cbind(A,numero.eleitos.A,numero.eleitos.B,numero.eleitos.C)
M = mutate(M, Total = eleitos.A + eleitos.B + eleitos.C)
