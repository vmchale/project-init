//! Module containing functions for rendering templates
extern crate rustache;

use std::fs;
use std::fs::File;
use std::io::Cursor;
use self::rustache::{HashBuilder, Render};
use std::io::prelude::*;
use std::os::unix::fs::PermissionsExt;

// Trait allowing us to create dirs/templates/files.
pub trait Create {
    fn create_dirs(&self, name: &str) -> ();
}

// Create directories given a Vec<String> of directory names
impl <T:ToString>Create for Vec<T> {
    fn create_dirs(&self, name: &str) -> () {
    self.into_iter()
        .map(|dir| { let mut subdir = name.to_string() ;
            subdir.push('/') ;
            subdir.push_str(&dir.to_string()) ;
            fs::create_dir(subdir) } ).count();
    }
}

///! render an <Vec<String> of templates, or do nothing
pub fn render_templates(project: &str, name: &str, hash: &HashBuilder, templates_pre: Option<Vec<String>>, executable: bool) -> () {
    if let Some(t) = templates_pre {

        // create Vec<T> of paths to templates
        let templates: Vec<String> = t.clone().into_iter()
            .map(|file| { let mut p = project.to_string();
                p.push('/');
                p.push_str(&file);
                p } ).collect();

        // read all the template files
        let template_files: Vec<String> = templates.into_iter()
            .map(|p| { 
                let template_f = File::open(p) ;
                let mut t = String::new();
                let s = "Failed to open file".to_string();
                template_f.expect(&s)
                    .read_to_string(&mut t)
                    .expect("File read failed.");
                t }).collect();

        // create Vec<T> of paths to rendered templates
        let templates_new: Vec<String> = t.into_iter()
            .map(|file| { let mut p = name.to_string();
                p.push('/');
                p.push_str(&file);
                p }).collect();

        // subtitute into template names
        let templates_named: Vec<String> = templates_new.into_iter()
                                   .map(|n| { let mut o = Cursor::new(Vec::new());
                                       hash.render(&n, &mut o).unwrap();
                                       String::from_utf8(o.into_inner()).unwrap() })
                                   .collect();

        // render all the template files
        let s: Vec<String> = template_files.clone().into_iter()
                                   .map(|file| { let mut o = Cursor::new(Vec::new());
                                       hash.render(&file, &mut o).unwrap();
                                       String::from_utf8(o.into_inner()).unwrap()})
                                   .collect();

        // write the rendered templates
        let files_to_write = templates_named.iter().zip(s.iter());
        let _ = files_to_write.into_iter()
            .map(|(path, contents)| { 
                let mut c = File::create(path)
                    .expect("File create failed.");
                let _ = c.write(contents.as_bytes());
                if executable {
                        let mut p = fs::metadata(path).expect("failed to read file metadata")
                            .permissions();
                        p.set_mode(0o755);
                        let _ = fs::set_permissions(path, p);
                };
            }
            ).count();
    }
}

pub fn render_file(license: &'static str, name: &str, filename: &str, hash: &HashBuilder) -> () {
    // render the template
    let mut o = Cursor::new(Vec::new());
    hash.render(&license, &mut o).unwrap();
    let contents = String::from_utf8(o.into_inner()).unwrap();

    // write the file
    let mut p = name.to_string();
    p.push('/');
    p.push_str(filename);
    let mut c = File::create(p)
        .expect("File create failed.");
    let _ = c.write(contents.as_bytes());

}
