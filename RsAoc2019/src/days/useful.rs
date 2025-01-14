pub mod Useful 
    {

    pub fn gcd(a: u64, b: u64) -> u64 {
    if a == 0 {
        b
    } else if b == 0 {
        a
    } else {
        let (mut a, mut b) = if a < b { (a, b) } else { (b, a) };
        while b != 0 {
            let r = a % b;
            a = b;
            b = r;
        }
        a
    }
}
    pub fn lcm(a: u64, b: u64) -> u64 {
        a * b / (gcd(a,b))
    }

}
