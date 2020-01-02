
fn switch<F, A, B, C>(f: F) -> impl Fn(A) -> Result<B, C>
    where F: Fn(A) -> B
{
    move |x: A| Ok(f(x))
}

#[derive(Debug)]
struct Request {
    name: String,
    email: String,
}

impl Request {
    
    fn err_from_str(s: &str) -> Result<Self, String> {
        Err(String::from(s))
    }

    fn validate_name(self) -> Result<Self, String> {
        if self.name == "" {
            Self::err_from_str("Name must not be blank")
        } else {
            Ok(self)
        }
    }
    
    fn validate_name_len(self) -> Result<Self, String> {
        if self.name.len() > 50 {
            Self::err_from_str("Name must not be more than 50 chars")
        } else {
            Ok(self)
        }
    }

    fn validate_email(self) -> Result<Self, String> {
        if self.email == "" {
            Self::err_from_str("Email must not be blank")
        } else {
            Ok(self)
        }
    }

    fn combined_validation(self) -> Result<Self, String> {

        // TODO: add `plus` function to combine errors or
        // find more idiomatic approach

        self.validate_name()
            // and_then is basically bind
            .and_then(|r| r.validate_name_len())
            .and_then(|r| r.validate_email())
    }

    fn canonicalize_email(self) -> Self {
        Request {email: self.email.to_lowercase(), ..self}
    }

    fn usecase(self) -> Result<Self, String> {
        // this is a little wordy
        // self.combined_validation()
        //     .and_then(switch(|s: Self| s.canonicalize_email()))
        // this is a little better
        self.combined_validation()
            .map(|s| s.canonicalize_email())
    }

    pub fn new<S, U>(name: S, email: U) -> Result<Self, String>
        where S: Into<String>, U: Into<String> // multiple where clause just for fun
    {
        Request{name: name.into(), email: email.into()}
            .usecase()
    }
}

fn main() {
    println!("Hello, world!");
    let mut l = Vec::new();
    l.push(Request::new("Drew".to_string(), "Drew@Example.COM"));
    l.push(Request::new("Drew", ""));
    l.push(Request::new("", "Drew@Example.COM"));
    l.push(Request::new("", ""));
    l.push(Request::new("Drew jklsdfjksljkljklsdfjlksfjlsdkjflsdkjflskdjflkjdfjkdsjfldkjflsdkjf", "Drew@Example.COM"));
    l.push(Request::new("Drew jklsdfjksljkljklsdfjlksfjlsdkjflsdkjflskdjflkjdfjkdsjfldkjflsdkjf", ""));
    for r in l {
        match r {
            Ok(r) => println!("{:?}", r),
            Err(e) => println!("{}", e),
        }
    }
}
