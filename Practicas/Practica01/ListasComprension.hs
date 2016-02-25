--Lozada Mendez Ivan--
module ListasComprension where
	--a) func_a lista por comprension
	func_a n = [((2^x)-1) | x<-[0..n]]

	--b) func_b lista por comprension
	func_b n = [(((x*4)-1),(y*4)) | x<-[1..n], y<-[1..n], x == y]
	
	--c) func_c lista por comprension
	func_c n = [((x*7)+51) | x<-[0..n]]

	--d) Listas pitagoricas por comprension
	pitagoricas n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2 + y^2 == z^2]