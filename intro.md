# Org-mode Linking by ID: Giải pháp liên kết thông minh cho Emacs

## Giới thiệu

Khi làm việc với Org-mode trong Emacs, việc tạo liên kết chéo giữa các header là một nhu cầu thường xuyên. Tuy nhiên, các phương pháp truyền thống như sử dụng nội dung header hoặc CUSTOM_ID có những hạn chế đáng kể:

- **Liên kết theo nội dung header**: Khi header thay đổi, link sẽ bị hỏng
- **CUSTOM_ID**: Đòi hỏi phải suy nghĩ và đặt tên cho ID, không tự nhiên
- **Thiếu tính năng tìm kiếm**: Khó khăn khi tìm kiếm trong nhiều file

Để giải quyết những vấn đề này, tôi đã phát triển extension **Org-mode Linking by ID** - một giải pháp thông minh cho việc tạo liên kết chéo trong Org-mode.

## Mục đích và thiết kế

### Tại sao sử dụng ID?

Extension này sử dụng ID tự động được tạo bởi `org-id-get-create` (phím tắt `SPC m I`) thay vì nội dung header. Điều này mang lại những lợi ích:

1. **Tính ổn định**: ID không thay đổi khi nội dung header thay đổi
2. **Tự động hóa**: Không cần suy nghĩ về việc đặt tên ID
3. **Tính nhất quán**: Sử dụng cùng một hệ thống ID của Org-mode

### Kiến trúc thiết kế

Extension được chia thành 3 module chính:

1. **Liên kết trong file hiện tại**: Tìm và liên kết headers trong file đang mở
2. **Liên kết trong thư mục**: Tìm kiếm headers trong tất cả file .org trong thư mục
3. **Liên kết đối tượng**: Liên kết đến hình ảnh, bảng có tên

## Cài đặt

### Yêu cầu hệ thống

- Emacs 26.1 trở lên
- Org-mode
- Ivy (cho fuzzy search)
- Helm (cho tìm kiếm đối tượng)

### Cách cài đặt

#### Phương pháp 1: Copy trực tiếp

1. Clone repository:
```bash
git clone https://github.com/drringo/orgmode-linked-by-id.git
```

2. Thêm vào file config của bạn (`.emacs`, `init.el`, hoặc `config.el`):
```elisp
(load "~/path/to/orgmode-linked-by-id.lisp")
```

#### Phương pháp 2: Sử dụng use-package

```elisp
(use-package orgmode-linked-by-id
  :load-path "~/path/to/orgmode-linked-by-id"
  :config
  (require 'orgmode-linked-by-id))
```

#### Phương pháp 3: Từ GitHub Gist

1. Truy cập: https://gist.github.com/drringo/your-gist-id
2. Copy nội dung file `orgmode-linked-by-id.lisp`
3. Paste vào file config của bạn

## Hướng dẫn sử dụng

### Quy trình làm việc cơ bản

1. **Tạo ID cho header**: `SPC m I`
2. **Tạo liên kết**: Sử dụng các phím tắt tương ứng
3. **Chọn header**: Từ danh sách được hiển thị
4. **Tùy chỉnh**: Description của link nếu cần

### Tính năng 1: Liên kết trong file hiện tại

**Phím tắt**: `C-c l i` hoặc `SPC l i`

**Chức năng**:
- Tìm tất cả headers có ID trong file đang mở
- Hiển thị danh sách cho người dùng chọn
- Chèn liên kết ID vào vị trí con trỏ

**Ví dụ sử dụng**:
```org
* Header 1
:PROPERTIES:
:ID: abc123
:END:

* Header 2
:PROPERTIES:
:ID: def456
:END:

Nội dung tham chiếu đến [[id:abc123][Header 1]].
```

### Tính năng 2: Liên kết trong thư mục

**Phím tắt**: `C-c l f` hoặc `SPC l f`

**Chức năng**:
- Tìm headers có ID trong tất cả file .org trong thư mục
- Sử dụng Ivy với fuzzy search để tìm kiếm dễ dàng
- Không bao gồm thư mục con (để tránh tìm kiếm quá rộng)

**Ưu điểm**:
- Tìm kiếm nhanh với fuzzy matching
- Chọn thư mục tương tác
- Hiển thị tên file cùng với header

### Tính năng 3: Liên kết đối tượng (hình ảnh, bảng)

**Phím tắt**: `C-c l c` hoặc `SPC l c`

**Chức năng**:
- Tìm các đối tượng có `#+NAME` và `#+CAPTION`
- Hiển thị danh sách caption cho người dùng chọn
- Cho phép tùy chỉnh description của link

**Ví dụ sử dụng**:
```org
#+NAME: my-image
#+CAPTION: Hình ảnh mẫu
[[file:image.png]]

Liên kết đến [[my-image][Hình ảnh mẫu]].
```

## Bảng phím tắt đầy đủ

| Phím tắt | Chức năng | Mô tả |
|----------|-----------|-------|
| `C-c l i` | Chèn ID link trong file | Tìm headers trong file hiện tại |
| `SPC l i` | Chèn ID link trong file | (Doom Emacs) |
| `C-c l f` | Chèn ID link trong thư mục | Tìm headers trong thư mục |
| `SPC l f` | Chèn ID link trong thư mục | (Doom Emacs) |
| `C-c l c` | Chèn link đối tượng | Tìm hình ảnh/bảng có tên |
| `SPC l c` | Chèn link đối tượng | (Doom Emacs) |

## Ví dụ thực tế

### Kịch bản 1: Ghi chú học tập

```org
* Môn học: Lập trình Python
:PROPERTIES:
:ID: python-course
:END:

* Bài 1: Giới thiệu Python
:PROPERTIES:
:ID: python-intro
:END:

* Bài 2: Biến và kiểu dữ liệu
:PROPERTIES:
:ID: python-variables
:END:

Trong [[id:python-intro][Bài 1]], chúng ta đã học về cú pháp cơ bản.
Tiếp theo trong [[id:python-variables][Bài 2]], chúng ta sẽ tìm hiểu về biến.
```

### Kịch bản 2: Quản lý dự án

```org
* Dự án: Website công ty
:PROPERTIES:
:ID: company-website
:END:

* Task: Thiết kế UI
:PROPERTIES:
:ID: ui-design
:END:

* Task: Phát triển backend
:PROPERTIES:
:ID: backend-dev
:END:

[[id:ui-design][Thiết kế UI]] cần hoàn thành trước khi bắt đầu [[id:backend-dev][phát triển backend]].
```

## Lợi ích so với phương pháp truyền thống

### So với liên kết theo nội dung header

| Phương pháp truyền thống | Extension này |
|--------------------------|---------------|
| `[[*Header name][Header name]]` | `[[id:abc123][Header name]]` |
| Hỏng khi header thay đổi | Vẫn hoạt động |
| Khó tìm kiếm | Tìm kiếm dễ dàng |

### So với CUSTOM_ID

| CUSTOM_ID | ID tự động |
|-----------|------------|
| Cần suy nghĩ đặt tên | Tự động tạo |
| Có thể trùng lặp | Đảm bảo duy nhất |
| Khó quản lý | Quản lý tự động |

## Kết luận

Org-mode Linking by ID extension giải quyết hiệu quả các vấn đề thường gặp khi làm việc với liên kết trong Org-mode. Với thiết kế đơn giản nhưng mạnh mẽ, extension này:

- **Tăng hiệu suất làm việc**: Tìm kiếm và tạo liên kết nhanh chóng
- **Đảm bảo tính ổn định**: Liên kết không bị hỏng khi nội dung thay đổi
- **Dễ sử dụng**: Giao diện trực quan với fuzzy search
- **Linh hoạt**: Hỗ trợ nhiều loại liên kết khác nhau

## Hướng mở rộng

### Tính năng có thể phát triển thêm

1. **Liên kết đa cấp**: Tìm kiếm trong thư mục con
2. **Lịch sử liên kết**: Theo dõi các liên kết đã tạo
3. **Tự động cập nhật**: Cập nhật description khi header thay đổi
4. **Export hỗ trợ**: Tối ưu cho việc export sang PDF/HTML
5. **Tích hợp với Org-roam**: Hỗ trợ cho hệ thống note-taking
6. **Giao diện đồ họa**: Tạo giao diện trực quan cho việc quản lý liên kết

### Đóng góp

Dự án này được phát triển như một công cụ mã nguồn mở. Mọi đóng góp đều được chào đón:

- Báo cáo lỗi
- Đề xuất tính năng mới
- Cải thiện code
- Viết tài liệu

### Liên kết

- **Repository**: https://github.com/drringo/orgmode-linked-by-id
- **Issues**: https://github.com/drringo/orgmode-linked-by-id/issues
- **Discussions**: https://github.com/drringo/orgmode-linked-by-id/discussions

---

*Extension này được phát triển với mục đích cải thiện trải nghiệm làm việc với Org-mode. Hy vọng nó sẽ hữu ích cho cộng đồng Emacs và Org-mode.* 