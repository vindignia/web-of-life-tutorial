
println("Series 1/n^2")

# Print the multiplication talbe of 9
#list = 1:10
#for i in list
#    println("9 * $i = ",9*i)
#end

my_sum_inv_square(N) = sum(1. / (i*i) for i in 1:N)

theo = Float64(pi*pi / 6)
res = my_sum_inv_square(200000) # my_sum(1000000000)

println("res  = ", res)
println("pi*pi/6  = ", theo)
println("Delta  = ", res-theo)


