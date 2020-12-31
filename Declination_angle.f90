module Declination_angle

implicit none

real,parameter:: pi = 3.1415926536

contains
integer::year,month,day
if (((mod(year,4)==0).and.(mod(year,100)/=0)).or.(mod(year,400)==0)) 
then
temp = (n+284)*360*pi/(366*180)
else
temp = (n+284)*360*pi/(365*180)
end if
decangle = 23.45*sin(temp)
end function decangle
end module Declination_angle
