# Author: Katie Nutley
# Date: 2026-04-17
# Title: Accessing Meckling & Allan's Data via Google Docs API 

from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request
from googleapiclient.discovery import build
import pickle
import os
import csv
from datetime import datetime

# Scopes define what access we need
SCOPES = [
    'https://www.googleapis.com/auth/documents.readonly',
    'https://www.googleapis.com/auth/drive.readonly'
]

def authenticate():
    """Authenticate with Google"""
    creds = None
    
    # Check if we have saved credentials
    if os.path.exists('token.pickle'):
        with open('token.pickle', 'rb') as token:
            creds = pickle.load(token)
    
    # If no valid credentials, authenticate
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                'credentials.json',  # Your downloaded credentials file
                SCOPES
            )
            creds = flow.run_local_server(port=0)
        
        # Save credentials for future use
        with open('token.pickle', 'wb') as token:
            pickle.dump(creds, token)
    
    return creds

def get_folder_by_name(parent_folder_id, folder_name, creds):
    """Find a specific folder by name within a parent folder"""
    
    drive_service = build('drive', 'v3', credentials=creds)
    
    try:
        query = f"'{parent_folder_id}' in parents and name='{folder_name}' and mimeType='application/vnd.google-apps.folder' and trashed=false"
        results = drive_service.files().list(
            q=query,
            spaces='drive',
            fields='files(id, name)',
            pageSize=1
        ).execute()
        
        items = results.get('files', [])
        if items:
            return items[0]['id']
        else:
            print(f"  ✗ Subfolder '{folder_name}' not found")
            return None
            
    except Exception as e:
        print(f"  ✗ Error searching for folder '{folder_name}': {str(e)}")
        return None

def get_all_docs_in_folder(folder_id, creds, parent_path=""):
    """Recursively get all Google Docs from a folder and its subfolders"""
    
    drive_service = build('drive', 'v3', credentials=creds)
    doc_ids = []
    
    try:
        # Query for files in this folder
        query = f"'{folder_id}' in parents and trashed=false"
        results = drive_service.files().list(
            q=query,
            spaces='drive',
            fields='files(id, name, mimeType)',
            pageSize=1000
        ).execute()
        
        items = results.get('files', [])
        
        for item in items:
            item_path = f"{parent_path}/{item['name']}" if parent_path else item['name']
            
            # If it's a Google Doc, add it to our list
            if item['mimeType'] == 'application/vnd.google-apps.document':
                doc_ids.append({
                    'id': item['id'],
                    'title': item['name'],
                    'path': item_path
                })
                print(f"  Found document: {item_path}")
            
            # If it's a folder, recursively search it
            elif item['mimeType'] == 'application/vnd.google-apps.folder':
                print(f"  Entering subfolder: {item_path}")
                subfolder_docs = get_all_docs_in_folder(item['id'], creds, item_path)
                doc_ids.extend(subfolder_docs)
        
    except Exception as e:
        print(f"  ✗ Error accessing folder: {str(e)}")
    
    return doc_ids

def get_doc_text(doc_id, creds):
    """Extract full text from a Google Doc"""
    try:
        docs_service = build('docs', 'v1', credentials=creds)
        doc = docs_service.documents().get(documentId=doc_id).execute()
        
        # Extract all text from the document
        doc_text = ""
        if 'body' in doc and 'content' in doc['body']:
            for element in doc['body']['content']:
                if 'paragraph' in element:
                    for text_run in element['paragraph'].get('elements', []):
                        if 'textRun' in text_run:
                            doc_text += text_run['textRun']['content']
        
        return doc_text
    
    except Exception as e:
        print(f"  ✗ Error extracting document text: {str(e)}")
        return ""

def get_sentence_containing_text(doc_text, target_text, context_words=5):
    """
    Find the full sentence containing target_text.
    Falls back to target_text with surrounding context if sentence boundary not clear.
    """
    if not target_text or not doc_text:
        return target_text
    
    # Try to find the target text in the document
    target_index = doc_text.find(target_text)
    if target_index == -1:
        return target_text  # Text not found, return as-is
    
    # Find sentence boundaries (. ! ?)
    # Look backward for sentence start
    sentence_start = target_index
    for i in range(target_index - 1, -1, -1):
        if doc_text[i] in '.!?\n':
            sentence_start = i + 1
            break
    else:
        sentence_start = 0
    
    # Look forward for sentence end
    sentence_end = target_index + len(target_text)
    for i in range(target_index + len(target_text), len(doc_text)):
        if doc_text[i] in '.!?\n':
            sentence_end = i + 1
            break
    else:
        sentence_end = len(doc_text)
    
    # Extract and clean the sentence
    sentence = doc_text[sentence_start:sentence_end].strip()
    return sentence if sentence else target_text

def get_paragraph_containing_text(doc_text, target_text):
    """
    Find the full paragraph containing target_text.
    Paragraphs are delimited by double newlines or single newlines on their own line.
    Falls back to the sentence context if a paragraph boundary cannot be found.
    """
    if not target_text or not doc_text:
        return target_text

    target_index = doc_text.find(target_text)
    if target_index == -1:
        return target_text  # Text not found, return as-is

    # Walk backward to find the start of the paragraph.
    # A paragraph break is two or more consecutive newline characters, or
    # a newline that immediately follows another newline (blank line).
    para_start = 0
    i = target_index - 1
    while i >= 0:
        if doc_text[i] == '\n':
            # Check whether this newline is preceded by another newline
            # (i.e. there is a blank line separating paragraphs)
            if i > 0 and doc_text[i - 1] == '\n':
                para_start = i + 1
                break
        i -= 1

    # Walk forward to find the end of the paragraph.
    para_end = len(doc_text)
    i = target_index + len(target_text)
    while i < len(doc_text):
        if doc_text[i] == '\n':
            if i + 1 < len(doc_text) and doc_text[i + 1] == '\n':
                para_end = i
                break
        i += 1

    paragraph = doc_text[para_start:para_end].strip()
    return paragraph if paragraph else get_sentence_containing_text(doc_text, target_text)

def extract_comments_to_csv(doc_id, creds, output_filename=None):
    """Extract comments from a Google Doc and save to CSV format"""
    
    print("="*60)
    print("EXTRACTING COMMENTS TO CSV")
    print("="*60)
    
    # Get document title
    try:
        docs_service = build('docs', 'v1', credentials=creds)
        doc = docs_service.documents().get(documentId=doc_id).execute()
        doc_title = doc.get('title', 'Untitled')
        print(f"\n✓ Document title: {doc_title}")
    except Exception as e:
        print(f"\n✗ FAILED: Cannot read document")
        print(f"  Error: {str(e)}")
        return False
    
    # Get full document text for context
    print("  Retrieving document text for context...")
    doc_text = get_doc_text(doc_id, creds)
    
    # Extract comments
    try:
        drive_service = build('drive', 'v3', credentials=creds)
        comments_result = drive_service.comments().list(
            fileId=doc_id,
            fields='comments(id,content,quotedFileContent,author,createdTime)',
            pageSize=100  # Adjust if you have many comments
        ).execute()
        
        comments = comments_result.get('comments', [])
        print(f"✓ Found {len(comments)} comments")
        
    except Exception as e:
        print(f"\n✗ FAILED: Cannot read comments")
        print(f"  Error: {str(e)}")
        return False
    
    # Prepare CSV output
    if output_filename is None:
        output_filename = f"comments_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv"
    
    try:
        with open(output_filename, 'w', newline='', encoding='utf-8') as csvfile:
            fieldnames = [
                'comment_text',
                'referenced_text',
                'full_sentence_context',
                'full_paragraph_context',  # NEW
                'document_title',
                'author',
                'created_time'
            ]
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            
            writer.writeheader()
            
            for comment in comments:
                comment_text = comment.get('content', '')
                referenced_text = comment.get('quotedFileContent', {}).get('value', '')
                full_sentence = get_sentence_containing_text(doc_text, referenced_text)
                full_paragraph = get_paragraph_containing_text(doc_text, referenced_text)  # NEW
                author = comment.get('author', {}).get('displayName', 'Unknown')
                created_time = comment.get('createdTime', '')
                
                writer.writerow({
                    'comment_text': comment_text,
                    'referenced_text': referenced_text,
                    'full_sentence_context': full_sentence,
                    'full_paragraph_context': full_paragraph,  # NEW
                    'document_title': doc_title,
                    'author': author,
                    'created_time': created_time
                })
        
        print(f"\n✓ SUCCESS: Comments exported to '{output_filename}'")
        print(f"  Rows written: {len(comments)}")
        return True
        
    except Exception as e:
        print(f"\n✗ FAILED: Cannot write CSV file")
        print(f"  Error: {str(e)}")
        return False

def extract_multiple_docs_to_csv(doc_ids, creds, output_filename=None):
    """Extract comments from multiple Google Docs and combine into single CSV"""
    
    print("="*60)
    print("EXTRACTING COMMENTS FROM MULTIPLE DOCUMENTS")
    print("="*60)
    
    if output_filename is None:
        output_filename = f"all_comments_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv"
    
    all_rows = []
    
    for idx, doc_info in enumerate(doc_ids, 1):
        doc_id = doc_info['id']
        doc_title = doc_info['title']
        doc_path = doc_info['path']
        
        print(f"\n[{idx}/{len(doc_ids)}] Processing document...")
        print(f"  Path: {doc_path}")
        
        # Get full document text for context
        doc_text = get_doc_text(doc_id, creds)
        
        # Extract comments
        try:
            drive_service = build('drive', 'v3', credentials=creds)
            comments_result = drive_service.comments().list(
                fileId=doc_id,
                fields='comments(id,content,quotedFileContent,author,createdTime)',
                pageSize=100
            ).execute()
            
            comments = comments_result.get('comments', [])
            print(f"  ✓ Found {len(comments)} comments")
            
            for comment in comments:
                referenced_text = comment.get('quotedFileContent', {}).get('value', '')
                full_sentence = get_sentence_containing_text(doc_text, referenced_text)
                full_paragraph = get_paragraph_containing_text(doc_text, referenced_text)  # NEW
                
                all_rows.append({
                    'comment_text': comment.get('content', ''),
                    'referenced_text': referenced_text,
                    'full_sentence_context': full_sentence,
                    'full_paragraph_context': full_paragraph,  # NEW
                    'document_title': doc_title,
                    'document_path': doc_path,
                    'author': comment.get('author', {}).get('displayName', 'Unknown'),
                    'created_time': comment.get('createdTime', '')
                })
            
        except Exception as e:
            print(f"  ✗ FAILED: Cannot read comments - {str(e)}")
            continue
    
    # Write combined CSV
    try:
        with open(output_filename, 'w', newline='', encoding='utf-8') as csvfile:
            fieldnames = [
                'comment_text',
                'referenced_text',
                'full_sentence_context',
                'full_paragraph_context',  # NEW
                'document_title',
                'document_path',
                'author',
                'created_time'
            ]
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            
            writer.writeheader()
            writer.writerows(all_rows)
        
        print(f"\n" + "="*60)
        print(f"✓ SUCCESS: All comments exported to '{output_filename}'")
        print(f"  Total rows written: {len(all_rows)}")
        print("="*60)
        return True
        
    except Exception as e:
        print(f"\n✗ FAILED: Cannot write CSV file")
        print(f"  Error: {str(e)}")
        return False

# Main execution
if __name__ == "__main__":
    print("Authenticating with Google...")
    creds = authenticate()
    
    # Your parent folder ID
    parent_folder_id = "13ysTuua6i5wWpyb5dmJWF_gD5ZiLN_16"
    
    # List of subfolder names to process
    subfolder_names = [
        "UNEP",
        "G7-8 Summit Communique",
        "UNDP",
        "WB Flagship",
        "OECD Flagship",
        "UNFCCC"
    ]
    
    all_docs = []
    
    print(f"\nSearching for documents in specified subfolders...\n")
    
    for subfolder_name in subfolder_names:
        print(f"Processing subfolder: {subfolder_name}")
        folder_id = get_folder_by_name(parent_folder_id, subfolder_name, creds)
        
        if folder_id:
            print(f"  ✓ Found folder")
            docs = get_all_docs_in_folder(folder_id, creds, subfolder_name)
            all_docs.extend(docs)
            print(f"  Found {len(docs)} documents\n")
        else:
            print(f"  Skipping subfolder\n")
    
    if all_docs:
        print(f"\n✓ Found {len(all_docs)} documents total")
        print("\nExtracting comments from all documents...")
        success = extract_multiple_docs_to_csv(all_docs, creds)
        
        if success:
            print("\n🎉 CSV file ready for BERT analysis!")
        else:
            print("\n⚠️  Failed to extract comments")
    else:
        print("\n⚠️  No documents found in the specified subfolders")
    
    # Alternative: Process a single document by ID
    # doc_id = "1u2V6ec9QQlPVTHGw3voJ8ev3LG4aYDA7lYg-QZ5FdJ4"
    # print("\nExtracting comments from document...")
    # success = extract_comments_to_csv(doc_id, creds)
